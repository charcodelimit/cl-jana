;;;
;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :io.zip.checksums)

(declaim (inline crc32))
(defun crc32 (high low buf start count)
  (declare #.*fixnum-optimize-settings*
           (type (unsigned-byte 16) high low)
           (type fixnum start count)
           (type (vector (octet) *) buf)
           (optimize speed))
  (let ((i start)
        (table *crc32-table*))
    (declare (type fixnum i)
             (type (simple-array (unsigned-byte 16) (*)) table))
    (dotimes (j count (values high low))
      (let ((index (logxor (logand low #xFF) (aref buf i))))
        (declare (type (integer 0 255) index))
        (let ((high-index (ash index 1))
              (low-index (1+ (ash index 1))))
          (declare (type (integer 0 511) high-index low-index))
          (let ((t-high (aref table high-index))
                (t-low (aref table low-index)))
            (declare (type (unsigned-byte 16) t-high t-low))
            (incf i)
            (setf low (logxor (ash (logand high #xFF) 8)
                              (ash low -8)
                              t-low))
            (setf high (logxor (ash high -8) t-high))))))))

;;; Class interface

(defclass crc32-checksum (checksum)
  ((low
    :initarg :low
    :accessor low)
   (high
    :initarg :high
    :accessor high))
  (:default-initargs
   :low #xFFFF
   :high #xFFFF))

(defmethod update ((checksum crc32-checksum) input start count)
  (declare #.*standard-optimize-settings*
           (type (vector (octet) *) input)
           (type fixnum start count))
  (setf (values (high checksum)
                (low checksum))
        (crc32 (high checksum) (low checksum)
               input start count)))

(defmethod result ((checksum crc32-checksum))
  (declare #.*fixnum-optimize-settings*)
  (+ (ash (logxor (high checksum) #xFFFF) 16)
     (logxor (low checksum) #xFFFF)))
     
;;; chr: this is now the responsibility of io.streams !
;(defmethod result-octets ((checksum crc32-checksum))
;  (ub32-octets (result checksum)))

(defmethod reset ((checksum crc32-checksum))
  (declare #.*standard-optimize-settings*)
  (setf (low checksum) #xFFFF
        (high checksum) #xFFFF))
