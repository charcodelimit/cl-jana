;;; -*- Mode: Lisp; Package: io.streams -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE:               integer.lisp
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

;; actual low-level conversion functions

(declaim (inline read-little-endian-unsigned))
(defun read-little-endian-unsigned (word-size input-stream)
  "The least significant byte is the first one in the INPUT-STREAM."
  (declare #.*fixnum-optimize-settings*
           (type stream input-stream)
	   (type fixnum word-size))
  (let ((sum 0)
	(byte 0))
    (declare (type fixnum sum word-size)
	     (type octet byte))
    (dotimes (i word-size sum)
      (setq byte
	    (read-byte input-stream))
      (incf sum
	    (ash byte (ash i 3))))))

(declaim (inline read-big-endian-unsigned))
(defun read-big-endian-unsigned (word-size input-stream)
  "The least significant byte is the first one in the INPUT-STREAM."
  (declare #.*fixnum-optimize-settings*
           (type stream input-stream)
	   (type fixnum word-size))
  (let ((sum 0)
	(num-octets (1- word-size))
	(byte 0))
    (declare (type fixnum sum num-octets)
	     (type octet byte))
    (dotimes (i word-size sum)
      (setq byte
	    (read-byte input-stream))
      (incf sum
	    (ash byte (ash (- num-octets i) 3))))))

(declaim (inline write-little-endian-unsigned))
(defun write-little-endian-unsigned (int word-size output-stream)
  "Write INT to OUTPUT-STREAM encoded as a little-endian unsigned integer
that is WORD-SIZE many octets wide."
  (declare #.*fixnum-optimize-settings*
           (type fixnum int word-size)
	   (type stream output-stream))
  (dotimes (i word-size)
    (write-byte (ldb (byte 8 (ash i 3)) int)
		output-stream)))

(declaim (inline write-big-endian-unsigned))
(defun write-big-endian-unsigned (int word-size output-stream)
  "Write INT to OUTPUT-STREAM encoded as a big-endian unsigned integer
that is WORD-SIZE many octets wide."
  (declare #.*fixnum-optimize-settings*
           (type fixnum int word-size)
	   (type stream output-stream))
    (let ((num-octets (1- word-size)))
      (declare (type fixnum num-octets))
    (dotimes (i word-size)
      (write-byte (ldb (byte 8 (ash (- num-octets i) 3)) int)
		  output-stream))))

(declaim (inline little-endian-octet-vector-to-unsigned))
(defun little-endian-octet-vector-to-unsigned (word-size octet-vector &key (offset 0))
  "The least significant byte is the first one in the vector OCTET-VECTOR."
  (declare #.*fixnum-optimize-settings*
           (type (vector byte *) octet-vector)
	   (type fixnum word-size))
  (let ((sum 0))
    (declare (type fixnum sum))
    (do ((pos offset (1+ pos))
	 (idx 0 (1+ idx)))
	((= idx word-size) sum)
      (incf sum
	    (ash (elt octet-vector pos) (ash idx 3))))))

(declaim (inline big-endian-octet-vector-to-unsigned))
(defun big-endian-octet-vector-to-unsigned (word-size octet-vector  &key (offset 0))
  "The least significant byte is the first one in the vector OCTET-VECTOR."
  (declare #.*fixnum-optimize-settings*
           (type (vector byte *) octet-vector)
	   (type fixnum word-size))
  (let ((sum 0)
	(num-octets (1- word-size)))
    (declare (type fixnum sum num-octets))
    (do ((pos offset (1+ pos))
	 (idx 0 (1+ idx)))
	((= idx word-size) sum)
      (incf sum
	    (ash (elt octet-vector pos) (ash (- num-octets idx) 3))))))

(declaim (inline integer-to-little-endian-octet-vector))
(defun integer-to-little-endian-octet-vector (int-value word-size)
  "Create an octet vector of size WORD-SIZE from the integer INT-VALUE."
  (declare #.*fixnum-optimize-settings*
           (type fixnum int-value word-size))
  (let ((octet-vector (make-array word-size :element-type 'octet)))
    (declare (type (vector octet *) octet-vector))
    (dotimes (i word-size octet-vector)
      (setf (elt octet-vector i)
	    (ldb (byte 8 (ash i 3)) int-value)))))

(declaim (inline integer-to-big-endian-octet-vector))
(defun integer-to-big-endian-octet-vector (int-value word-size)
  "Create an octet vector of size WORD-SIZE from the integer INT-VALUE."
  (declaim #.*fixnum-optimize-settings*
           (type fixnum int-value word-size))
  (let ((octet-vector (make-array word-size :element-type 'octet))
	(num-octets (1- word-size)))
    (declare (type fixnum num-octets)
	     (type (vector octet *) octet-vector))
    (dotimes (i word-size octet-vector)
      (setf (elt octet-vector i)
	    (ldb (byte 8 (ash (- num-octets i) 3)) int-value)))))


