;;; -*- Mode: Lisp; Package: io.streams -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               output.lisp
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

(in-package :io.streams)

(defgeneric write-value (value number-encoding ouptut-stream)
    (declare #.*standard-optimize-settings*)
    (:DOCUMENTATION "Writes the value VALUE of a number to the stream
OUTPUT-STREAM using the encoding specified by NUMBER-ENCODING."))

;;; Numbers

(defmethod write-value ((value number-encoding) (self number-encoding) (output-stream stream))
  "Write the VALUE of an unsigned integer to OUTPUT-STREAM
 using an unsigned-integer encoding SELF."
  (declare #.*standard-optimize-settings*)
  (let ((endianess (slot-value self 'endianess))
	(word-size (slot-value self 'word-size)))
    (cond
      ((eq endianess :little)
       (the integer (write-little-endian-unsigned value word-size output-stream)))
      ((eq endianess :big)
       (the integer (write-big-endian-unsigned value word-size output-stream)))
      (t (error "Unsupported Endianess!")))))

;;; Integers

(defmethod write-value ((value integer) (self twos-complement-signed-integer-encoding) (output-stream stream))
  "Write the VALUE of a signed integer to OUTPUT-STREAM
 using a two's-complement integer encoding SELF."
  (declare #.*standard-optimize-settings*)
  (unless (<= (min-value self) value (max-value self))
    (error "The value ~A is out of range for encoding ~A." value self))
  (if (< value 0)
      (call-next-method (+ value (modulo self)) self output-stream)
      (call-next-method value self output-stream)))

(defmethod write-value ((value integer) (self ones-complement-signed-integer-encoding) (output-stream stream))
  "Write the VALUE of a signed integer to OUTPUT-STREAM
 using a one's-complement integer encoding SELF."
  (declare #.*standard-optimize-settings*)
  (unless (<= (min-value self) value (max-value self))
    (error "The value ~A is out of range for encoding ~A." value self))
  (if (< value 0)
      (call-next-method (+ value -1 (modulo self)) self output-stream)
      (call-next-method value self output-stream)))

;;; BCD Numbers

(defmethod write-value ((value integer) (self binary-coded-decimal-encoding) (output-stream stream))
  "Write the VALUE of a signed integer to OUTPUT-STREAM
 using a binary-coded-decimal encoding SELF."
  (declare #.*standard-optimize-settings*)
  (unless (<= (min-value self) value (max-value self))
    (error "The value ~A is out of range for encoding ~A." value self))
  (call-next-method (integer-to-binary-coded-decimal value) self output-stream))

;;; Floating Point Numbers

(defmethod write-value (value (self ieee-float-single-encoding) (output-stream stream))
  "Write the VALUE of a floating point number to OUTPUT-STREAM
 using an IEEE 754 float 32-bit single precision encoding SELF.
The method also accepts the values :not-a-number and :negative-infinity."
  (declare #.*standard-optimize-settings*)
  (call-next-method (encode-float32 value) self output-stream))

(defmethod write-value (value (self ieee-float-double-encoding) (output-stream stream))
  "Write the VALUE of a floating point number to OUTPUT-STREAM
 using an IEEE 754 float 64-bit double precision encoding SELF.
The method also accepts the values :not-a-number and :negative-infinity."
  (declare #.*standard-optimize-settings*)
  (call-next-method (encode-float64 value) self output-stream))

