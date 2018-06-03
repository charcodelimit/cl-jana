;;;; stream.lisp -- gray stream wrappers for INFLATE

(in-package :chipz)


;;; portability definitions

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

;;; TRIVIAL-GRAY-STREAMS has it, we might as well, too...
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc.fasl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *binary-input-stream-class*
  (quote
   #+lispworks stream:fundamental-binary-input-stream
   #+sbcl sb-gray:fundamental-binary-input-stream
   #+openmcl gray:fundamental-binary-input-stream
   #+cmu ext:fundamental-binary-input-stream
   #+allegro excl:fundamental-binary-input-stream
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-read-byte-function*
  (quote
   #+lispworks stream:stream-read-byte
   #+sbcl sb-gray:stream-read-byte
   #+openmcl gray:stream-read-byte
   #+cmu ext:stream-read-byte
   #+allegro excl:stream-read-byte
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))
) ; EVAL-WHEN

;;; class definition

(defclass decompressing-stream (#.*binary-input-stream-class*)
  ((wrapped-stream :initarg :stream :reader wrapped-stream)
   (dstate :initarg :dstate :reader dstate)
   (dfun :initarg :dfun :reader dfun)
   (input-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
                 :reader input-buffer)
   (input-buffer-index :initform 0 :accessor input-buffer-index)
   (input-buffer-n-bytes :initform 0 :accessor input-buffer-n-bytes)
   (output-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
                  :reader output-buffer)
   (output-buffer-index :initform 0 :accessor output-buffer-index)
   (output-buffer-n-bytes :initform 0 :accessor output-buffer-n-bytes)))

;;; constructors
(defun make-decompressing-stream (format stream)
  (multiple-value-bind (state dfun)
      (ecase format
        ((:deflate :zlib :gzip deflate zlib gzip)
         (values (make-inflate-state format) #'%inflate))
        ((:bzip2 bzip2)
         (values (make-bzip2-state) #'%bzip2-decompress)))
    (make-instance 'decompressing-stream
                   :stream stream
                   :dstate state
                   :dfun dfun)))


;;; stream management

(defun output-available-p (stream)
  (/= (output-buffer-index stream) (output-buffer-n-bytes stream)))

(defun input-available-p (stream)
  (/= (input-buffer-index stream) (input-buffer-n-bytes stream)))

(defun refill-stream-input-buffer (stream)
  (with-slots (input-buffer wrapped-stream
                            input-buffer-index input-buffer-n-bytes)
      stream
    (let ((n-bytes-read (read-sequence input-buffer wrapped-stream)))
      (setf input-buffer-index 0 input-buffer-n-bytes n-bytes-read)
      #+nil
      (format *trace-output* "index: ~D | n-bytes ~D~%"
              input-buffer-index input-buffer-n-bytes)
      (values))))

(defun refill-stream-output-buffer (stream)
  (unless (input-available-p stream)
    (refill-stream-input-buffer stream))
  (multiple-value-bind (bytes-read bytes-output)
      (funcall (the function (dfun stream))
               (dstate stream)
               (input-buffer stream)
               (output-buffer stream)
                :input-start (input-buffer-index stream)
                :input-end (input-buffer-n-bytes stream))
    (setf (output-buffer-index stream) 0
          (output-buffer-n-bytes stream) bytes-output
          (input-buffer-index stream) (+ (input-buffer-index stream) bytes-read))
    (assert (<= (input-buffer-index stream) (input-buffer-n-bytes stream)))))


;;; methods

(defun read-and-decompress-byte (stream)
  (unless (output-available-p stream)
    (refill-stream-output-buffer stream))
  ;; FIXME: should we cache this, so we don't try to refill all the time?
  (cond
    ((output-available-p stream)
     (prog1 (aref (output-buffer stream) (output-buffer-index stream))
       (incf (output-buffer-index stream))))
    (t
     (finish-dstate (dstate stream))
     :eof)))

(defmethod #.*stream-read-byte-function* ((stream decompressing-stream))
  (read-and-decompress-byte stream))
