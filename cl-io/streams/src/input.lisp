;;; -*- Mode: Lisp; Package: io.streams -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               input.lisp
;;; LANGUAGE:           Common-Lisp
;;;
;;; DESCRIPTION
;;;
;;;
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

;; + io
;;   + sequences -- byte buffers
;;   |
;;   + streams -- stream ops
;;   |
;;   + encodings -- encodings
;;

(in-package :io.streams)

;; octet-vector operations allow to use byte-buffers

(defgeneric get-value (number-encoding vector &key offset)
  (declare #.*standard-optimize-settings*)
  (:DOCUMENTATION "RETURNS the value of a number that was stored
in octet-vector VECTOR using the specified encoding NUMBER-ENCODING."))

(defgeneric read-value (number-encoding input-stream)
  (declare #.*standard-optimize-settings*)
  (:DOCUMENTATION "Reads the value of a number from the
underlying stream INPUT-STREAM and decodes it according to
the specified encoding NUMBER-ENCODING."))

;;; Numbers

(defmethod get-value ((self number-encoding) (octet-vector vector) &key (offset 0))
  "RETURNS the value of an unsigned integer stored in OCTET-VECTOR
 with an unsigned-integer encoding SELF."
  (declare #.*standard-optimize-settings*
           (type (vector octet *) octet-vector)
	   (type fixnum offset))
  (let ((endianess (slot-value self 'endianess))
	(word-size (slot-value self 'word-size)))
    (cond
      ((eq endianess :little)
       (the integer (little-endian-octet-vector-to-unsigned word-size octet-vector :offset offset)))
      ((eq endianess :big)
       (the integer (big-endian-octet-vector-to-unsigned word-size octet-vector :offset offset)))
      (t (error "Unsupported Endianess!")))))

(defmethod read-value ((self number-encoding) (input-stream stream))
  "RETURNS the value of an unsigned integer read from INPUT-STREAM
 using an unsigned-integer encoding SELF."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((endianess (slot-value self 'endianess))
	(word-size (slot-value self 'word-size)))
    (declare (type integer word-size))
    (cond
      ((eq endianess :little)
       (the integer (read-little-endian-unsigned word-size input-stream)))
      ((eq endianess :big)
       (the integer (read-big-endian-unsigned word-size input-stream)))
      (t (error "Unsupported Endianess!")))))

;;; Integers

(defmethod get-value ((self twos-complement-signed-integer-encoding) (octet-vector vector) &key (offset 0))
  "RETURNS the value of a signed integer stored in OCTET-VECTOR
 using a two's-complement integer encoding SELF."
  (declare #.*standard-optimize-settings*
           (type (vector octet *) octet-vector)
	   (type fixnum offset))
  (let ((unsigned-value (call-next-method self octet-vector :offset offset)))
    (if (< unsigned-value (ash (modulo self) -1))
	unsigned-value
	(- unsigned-value (modulo self)))))

(defmethod get-value ((self ones-complement-signed-integer-encoding) (octet-vector vector) &key (offset 0))
  "RETURNS the value of a signed integer stored in OCTET-VECTOR
 using a one's-complement integer encoding SELF."
  (declare #.*standard-optimize-settings*
           (type (vector octet *) octet-vector)
	   (type fixnum offset))
  (let ((unsigned-value (call-next-method self octet-vector :offset offset)))
    (if (< unsigned-value (ash (modulo self) -1))
	unsigned-value
	(- unsigned-value -1 (modulo self)))))

(defmethod read-value ((self twos-complement-signed-integer-encoding) (input-stream stream))
  "RETURNS the value of a signed integer read from INPUT-STREAM
 using a two's-complement integer encoding SELF."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((unsigned-value (call-next-method self input-stream)))
    (if (< unsigned-value (ash (modulo self) -1))
	unsigned-value
	(- unsigned-value (modulo self)))))

(defmethod read-value ((self ones-complement-signed-integer-encoding) (input-stream stream))
  "RETURNS the value of a signed integer read from INPUT-STREAM
 using a one's-complement integer encoding SELF."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((unsigned-value (call-next-method self input-stream)))
    (if (< unsigned-value (ash (modulo self) -1))
	unsigned-value
	(- unsigned-value -1 (modulo self)))))

;;; BCD-Numbers

(defmethod get-value ((self binary-coded-decimal-encoding) (octet-vector vector) &key (offset 0))
  "RETURNS the value of a signed integer storded in OCTET-VECTOR
 using a binary-coded-decimal encoding SELF."
  (declare #.*standard-optimize-settings*
           (type (vector octet *) octet-vector)
	   (type fixnum offset))
  (let ((unsigned-value (call-next-method self octet-vector offset)))
    (binary-coded-decimal-to-integer unsigned-value)))

(defmethod read-value ((self binary-coded-decimal-encoding) (input-stream stream))
  "RETURNS the value of a signed integer read from INPUT-STREAM
 using a binary-coded-decimal encoding SELF."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((unsigned-value (call-next-method self input-stream)))
    (binary-coded-decimal-to-integer unsigned-value)))

;; Floating Point Numbers

(defmethod get-value ((self ieee-float-single-encoding) (octet-vector vector) &key (offset 0))
  "RETURNS the value of a single-float storded in OCTET-VECTOR
 using an IEEE 754 float 32-bit single precision encoding SELF."
  (declare #.*standard-optimize-settings*
           (type (vector octet *) octet-vector)
	   (type fixnum offset))
  (let ((unsigned-value (call-next-method self octet-vector offset)))
    (decode-float-32 unsigned-value)))

(defmethod read-value ((self ieee-float-single-encoding) (input-stream stream))
  "RETURNS the value of a single-float read from INPUT-STREAM
 using an IEEE 754 float 32-bit single precision encoding SELF."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((unsigned-value (call-next-method self input-stream)))
    (decode-float-32 unsigned-value)))

(defmethod get-value ((self ieee-float-double-encoding) (octet-vector vector) &key (offset 0))
  "RETURNS the value of a double-float storded in OCTET-VECTOR
 using an IEEE 754 float 64-bit double precision encoding SELF."
  (declare #.*standard-optimize-settings*
           (type (vector octet *) octet-vector)
	   (type fixnum offset))
  (let ((unsigned-value (call-next-method self octet-vector offset)))
    (decode-float-64 unsigned-value)))

(defmethod read-value ((self ieee-float-double-encoding) (input-stream stream))
  "RETURNS the value of a double-float read from INPUT-STREAM
 using an IEEE 754 float 64-bit double precision encoding SELF."
  (declare #.*standard-optimize-settings*
           (type stream input-stream))
  (let ((unsigned-value (call-next-method self input-stream)))
    (decode-float-64 unsigned-value)))