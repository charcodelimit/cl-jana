;;; -*- Mode: Lisp; Package: io.streams -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               integer.lisp
;;; LANGUAGE:           Common-Lisp
;;;
;;; DESCRIPTION
;;;
;;;  Number-Encodings used to retrieve and store numbers from
;;;  octet-vectors and streams
;;;
;;;
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-05-29 <CHR> Initial Version.
;;; BUGS
;;; LEGAL
;;;    
;;;    Copyright C. Hofmann 2009
;;;    mailto:c.hofmann@utwente.nl
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the \"Software\"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the \"Software\"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;    
;;;***************************************************************************

(in-package :io.streams)

;; encoding class definitions

(defclass number-encoding ()
  ((name
    :READER number-encoding-name
    :INITARG :name
    :DOCUMENTATION "The name of the number encoding - a keyword.")
   (encoding
    :READER encoding
    :DOCUMENTATION "The name of the encoding - a keyword.")
   (word-size
    :READER word-size
    :INITARG :word-size
    :TYPE fixnum
    :DOCUMENTATION "The word-size in octets.")
   (endianess
    :READER endianess
    :INITARG :endianess
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "The abstract superclass for all number encodings."))

(defclass integer-encoding (number-encoding)
  ()
  (:DOCUMENTATION "The abstract superclass for all signed and unsigned integer encodings."))

(defclass  unsigned-integer-encoding (integer-encoding)
  ((encoding
    :READER encoding
    :INITARG :encoding    
    :INITFORM :unsigned
    :DOCUMENTATION "The name of the encoding - a keyword."))
  (:DOCUMENTATION "An unsigned integer encoding."))

(defclass  unsigned-integer-le-encoding (unsigned-integer-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :little
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "An unsigned little-endian integer encoding."))

(defclass  unsigned-integer-be-encoding (unsigned-integer-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :big
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "An unsigned big-endian integer encoding."))

(defclass  signed-integer-encoding (number-encoding)
  ((modulo
    :READER modulo
    :TYPE integer
    :DOCUMENTATION "The modulo for the signed integer encoding,
which is initialized once, when the instance is created."))
  (:DOCUMENTATION "A signed integer encoding."))


(defclass  twos-complement-signed-integer-encoding (signed-integer-encoding)
  ((encoding
    :READER encoding
    :INITARG :encoding    
    :INITFORM :twos-complement
    :DOCUMENTATION "The name of the encoding - a keyword."))
  (:DOCUMENTATION "A two's-complement signed integer encoding."))

(defclass  twos-complement-signed-integer-le-encoding (twos-complement-signed-integer-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :little
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "A little-endian two's-complement signed integer encoding."))

(defclass  twos-complement-signed-integer-be-encoding (twos-complement-signed-integer-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :big
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "A big-endian two's-complement signed integer encoding."))

(defclass  ones-complement-signed-integer-encoding (signed-integer-encoding)
  ((encoding
    :READER encoding
    :INITARG :encoding    
    :INITFORM :ones-complement
    :DOCUMENTATION "The name of the encoding - a keyword."))
  (:DOCUMENTATION "A one's-complement signed integer encoding."))

(defclass  ones-complement-signed-integer-le-encoding (ones-complement-signed-integer-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :little
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "A little-endian one's-complement signed integer encoding."))

(defclass  ones-complement-signed-integer-be-encoding (ones-complement-signed-integer-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :big
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "A big-endian one's-complement signed integer encoding."))

(defclass binary-coded-decimal-encoding (signed-integer-encoding)
  ((encoding
    :READER encoding
    :INITARG :encoding
    :INITFORM :bcd
    :DOCUMENTATION "The name of the encoding - a keyword."))
  (:DOCUMENTATION "A BCD number encoding."))

(defclass  binary-coded-decimal-le-encoding (binary-coded-decimal-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :little
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "A little-endian BCD number encoding."))

(defclass  binary-coded-decimal-be-encoding (binary-coded-decimal-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess
    :INITFORM :big
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
  (:DOCUMENTATION "A big-endian BCD number encoding."))

(defclass floating-point-number-encoding (number-encoding)
  ()
  (:DOCUMENTATION "An abstract class for floating-point number encodings."))

(defclass  ieee-float-single-encoding (floating-point-number-encoding)
  ((encoding
    :READER encoding
    :INITARG :encoding
    :INITFORM :ieee-float
    :DOCUMENTATION "The name of the encoding - a keyword.")
   (word-size
    :READER word-size
    :INITARG :word-size
    :INITFORM 4
    :TYPE fixnum
    :DOCUMENTATION "The word-size in octets."))
  (:DOCUMENTATION "An IEEE single-precision 32-bit floating point number encoding."))

(defclass  ieee-float-double-encoding (floating-point-number-encoding)
  ((encoding
    :READER encoding
    :INITARG :encoding
    :INITFORM :ieee-double
    :DOCUMENTATION "The name of the encoding - a keyword.")
   (word-size
    :READER word-size
    :INITARG :word-size
    :INITFORM 8
    :TYPE fixnum
    :DOCUMENTATION "The word-size in octets."))
  (:DOCUMENTATION "An IEEE double-precision 64-bit floating point number encoding."))

(defclass  ieee-float-single-le-encoding (ieee-float-single-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess    
    :INITFORM :little
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
   (:DOCUMENTATION "A little-endian IEEE single-precision 32-bit floating point number encoding."))

(defclass  ieee-float-single-be-encoding (ieee-float-single-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess    
    :INITFORM :big
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
   (:DOCUMENTATION "A big-endian IEEE single-precision 32-bit floating point number encoding."))

(defclass  ieee-float-double-le-encoding (ieee-float-double-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess    
    :INITFORM :little
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
   (:DOCUMENTATION "A little-endian IEEE double-precision 64-bit floating point number encoding."))

(defclass  ieee-float-double-be-encoding (ieee-float-double-encoding)
  ((endianess
    :READER endianess
    :INITARG :endianess    
    :INITFORM :big
    :DOCUMENTATION "The endianess of the encoding - a keyword."))
   (:DOCUMENTATION "A big-endian IEEE double-precision 64-bit floating point number encoding."))


;; constructor helper-definitions

(defun number-encoding-class-name (real-name &key encoding endianess word-size)
  "Given the initargs for a general number encoding returns the name
\(a symbol) of the most specific subclass matching these arguments."
  (declare (ignore word-size))
  (cond ((eq real-name :number)
	 (ecase encoding
           (:unsigned (if (eq endianess :little)
			  'unsigned-integer-le-encoding
			  'unsigned-integer-be-encoding))
           (:twos-complement (if (eq endianess :little)
				'twos-complement-signed-integer-le-encoding
				'twos-complement-signed-integer-be-encoding))
	   (:ones-complement (if (eq endianess :little)
				'ones-complement-signed-integer-le-encoding
				'ones-complement-signed-integer-be-encoding))
           (:bcd (if (eq endianess :little)
		     'binary-coded-decimal-le-encoding
		     'binary-coded-decimal-be-encoding))
	   (:ieee-float (if (eq endianess :little)
                            'ieee-float-single-le-encoding
                            'ieee-float-single-be-encoding))
	   (:ieee-double (if (eq endianess :little)
                             'ieee-float-double-le-encoding
                             'ieee-float-double-be-encoding))))
	((keywordp real-name)
	 (ecase real-name
	   (:u16 'unsigned-integer-encoding)
	   (:u32 'unsigned-integer-encoding)
	   (:u64 'unsigned-integer-encoding)
	   (:s16 'twos-complement-signed-integer-encoding)
	   (:s32 'twos-complement-signed-integer-encoding)
	   (:s64 'twos-complement-signed-integer-encoding)
	   (:flt 'ieee-float-single-le-encoding)
	   (:float 'ieee-float-single-le-encoding)
	   (:dbl 'ieee-float-double-le-encoding)
	   (:double 'ieee-float-double-le-encoding)
	   (:le16 'unsigned-integer-le-encoding)
	   (:le32 'unsigned-integer-le-encoding)
	   (:le64 'unsigned-integer-le-encoding)
	   (:be16 'unsigned-integer-be-encoding)
	   (:be32 'unsigned-integer-be-encoding)
	   (:be64 'unsigned-integer-be-encoding)
	   (:bcd16 'binary-coded-decimal-le-encoding)
	   (:bcd32 'binary-coded-decimal-le-encoding)
	   (:bcd64 'binary-coded-decimal-le-encoding)))
	(t 'number-encoding)))

(defun number-encoding-word-size (name)
  "Returns the word-size in octets for the number-encoding given by NAME."
  (cond
    ((keywordp name)
     (ecase name
       (:u16 2)
       (:u32 4)
       (:u64 8)
       (:s16 2)
       (:s32 4)
       (:s64 8)
       (:flt 4)
       (:float 4)
       (:dbl 8)
       (:double 8)
       (:le16 2)
       (:le32 4)
       (:le64 8)
       (:be16 2)
       (:be32 4)
       (:be64 8)
       (:bcd16 2)
       (:bcd32 4)
       (:bcd64 8)))
    (t (error "Unknown number-encoding. Please specify the word-size!"))))

(defun integer-endian-name-p (name)
  "TRUE if NAME is the name for an integer encoding with known endianess."
  (if (keywordp name)
      (member name '(:le16 :le32 :le64 :be16 :be32 :be64))
      nil))

(defun integer-name-p (name)
  "TRUE if NAME is the name for an integer encoding with unknown endianess."
  (if (keywordp name)
      (member name '(:u16 :u32 :u64 :s16 :s32 :s64))
      nil))

(defun bcd-name-p (name)
  "TRUE if NAME is the name for a bcd encoding with unknown endianess."
  (if (keywordp name)
      (member name '(:bcd16 :bcd32 :bcd64))
      nil))

(defun ieee-float-name-p (name)
  "TRUE if NAME is the name for an ieee floating-point number encoding."
  (if (keywordp name)
      (member name '(:float :double :flt :dbl))
      nil))

;; instance intialization

(defmethod initialize-instance ((self signed-integer-encoding) &rest args)
  "Initializes the modulo for signed integer encodings."
  (declare (ignore args))
  (call-next-method)
  (setf (slot-value self 'modulo)
	; 2 ^ bits
	(expt 2 (ash (slot-value self 'word-size) 3)))
  self)

(defmethod initialize-instance ((self binary-coded-decimal-encoding) &rest args)
  "Initialize the modulo for BCD-encodings."
  (declare (ignore args))
  (call-next-method)
  (setf (slot-value self 'modulo)
	; 10 ^ number of digits
	(expt 10 (1- (ash (slot-value self 'word-size) 1))))
  self)

;; constructor

(defun make-number-encoding (name &key encoding word-size endianess)
  (let ((initargs
	 (cond ((integer-endian-name-p name)
		(list :word-size (or word-size (number-encoding-word-size name))))
	       ((ieee-float-name-p name)
		(list :endianess (or endianess :little)))
	       ((or (integer-name-p name)
		    (bcd-name-p name))
		(list :endianess (or endianess :little)
		      :word-size (or word-size (number-encoding-word-size name))))
	       (t
		(list :encoding (or encoding :unsigned)
		      :endianess (or endianess :little)
		      :word-size (or word-size 2))))))
    (apply #'make-instance (apply #'number-encoding-class-name name initargs)
	   :name name
	   initargs)))

;; various methods

(defmethod max-value ((self signed-integer-encoding))
  (1- (ash (slot-value self 'modulo) -1)))

(defmethod min-value ((self signed-integer-encoding))
  (- (ash (slot-value self 'modulo) -1)))

(defmethod max-value ((self binary-coded-decimal-encoding))
  (slot-value self 'modulo))

(defmethod min-value ((self binary-coded-decimal-encoding))
  (- (slot-value self 'modulo)))

(defmethod num-digits ((self binary-coded-decimal-encoding))
  (1- (ash (slot-value self 'word-size) 1)))

(defun normalize-number-encoding (number-encoding)
  "Returns a list which is a `normalized' representation of the
external format NUMBER-ENCODING.  Used internally by PRINT-OBJECT, for
example.  Basically, the result is an argument list that can be fed
back to MAKE-NUMBER-ENCODING to create an equivalent object."
  (let ((name (number-encoding-name number-encoding))
        (encoding (encoding number-encoding))
        (word-size (word-size number-encoding))
        (endianess (endianess number-encoding)))
    (cond ((integer-endian-name-p name)
           (list name :word-size word-size))
          ((ieee-float-name-p name)
           (list name :endianess endianess))
          ((or (integer-name-p name)
               (bcd-name-p name))
           (list name :endianess endianess :word-size word-size))
          (t
           (list name :encoding encoding
		      :endianess endianess
		      :word-size word-size)))))          

(defmethod print-object ((object number-encoding) stream)
  "How an EXTERNAL-FORMAT object is rendered.  Uses
NORMALIZE-EXTERNAL-FORMAT."
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (normalize-number-encoding object) stream)))

(defmethod make-load-form ((self number-encoding) &optional environment)
  (declare (ignore environment))
  (let ((class (class-of self)))
  `(make-instance
    ',class
    :name ',(number-encoding-name self)
    :encoding ',(encoding self)
    :endianess ',(endianess self)
    :word-size ',(word-size self))))