;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-values.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Thu Aug  7 22:57:41 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:37 2010 (+0100)
;;;           By: Christian Hofmann
;;; 
;;; Copyright (C) 2008-2009, Christian Hofmann. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; 
;;;****************************************************************************

(in-package :JANA.JAVA.JIMPLE.CONVERSION)

(defmethod write-jimple-statement ((self java-null-value) stream)
   "RETURNS the string representation of a java-null-value that is used in the Jimple intermediate language."
   (declare (type stream stream))
   (write-sequence "null" stream))

(defmethod write-jimple-statement ((self java-constant-value) stream)
  "RETURNS the string representation that is used in the Jimple intermediate language of a java-constant-value ."
  (declare (type stream stream))
  (princ (value self) stream))

(declaim (inline write-floating-point-value))
(defun write-floating-point-value-to-string (value)
  "Because the syntax of the Java language requires that the exponent has to be denoted
 by the letter #\E, we convert valid lisp representations like 0.0S0,0.0D0 and 0.0L0 to
0.0E0"
  (declare (type float value))
  (let* ((string-representation (string-upcase (princ-to-string value)))
         (hash-value (sxhash string-representation))
         (result ""))
    (declare (type string string-representation result)
             (type fixnum hash-value))
    (setq result    
          (nsubstitute #\E #\S string-representation))
    (when (= (sxhash result) hash-value)
      (setq result    
            (nsubstitute #\E #\F string-representation)))
    (when (= (sxhash result) hash-value)
      (setq result
            (nsubstitute #\E #\D string-representation)))
    (when (= (sxhash result) hash-value)
      (setq result
            (nsubstitute #\E #\L string-representation)))
    result))

(defun write-floating-point-value (value stream)
  "Writes a floating point value VALUE to the output-stream STREAM.
Exponents are written verbatim, e.g. 0.0E0 is written as 0.0E0."
  (declare (type float value)
           (type stream stream))
  (write-sequence
   (write-floating-point-value-to-string value)
   stream))

(defun write-floating-point-value-without-exponent (value stream)
  "Writes a floating point value VALUE to the output-stream STREAM.
Unneccessary exponents are removed, e.g. 0.0E0 is written as 0.0."
  (declare (type float value)
           (type stream stream))
  (let ((string-representation (write-floating-point-value-to-string value)))
    (declare (type string string-representation))
    (write-sequence
     (subseq string-representation 0 (- (length string-representation) 2))
     stream)))

(defmethod write-jimple-statement ((self java-constant-fp-value) stream)
  "RETURNS the string representation of the java-constant-fp-value that is used in the Jimple intermediate language.
This is also the default for iava-constant-double-value,
which has the form: <number>"
  (declare (type stream stream))
  (cond ((slot-boundp self 'value)
	 (write-floating-point-value (value self) stream))
	((slot-boundp self 'out-of-range-value)
	 (write-sequence (out-of-range-value self) stream))))

(defmethod write-jimple-statement ((self java-constant-float-value) stream)
  "RETURNS the string representation that is used in the Jimple intermediate language of a java-constant-float-value .
This has the form: <number> 'F'"
  (declare (type stream stream))
  (call-next-method)
  (write-char #\F stream))

(defmethod write-jimple-statement ((self java-constant-exponent-free-double-value) stream)
  "RETURNS the string representation that is used in the Jimple intermediate language of a java-constant-float-value .
This has the form: <number> 'F'"
  (declare (type stream stream))
  (cond ((slot-boundp self 'value)
	 (write-floating-point-value-without-exponent (value self) stream))
	((slot-boundp self 'out-of-range-value)
	 (write-sequence (out-of-range-value self) stream))))

(defmethod write-jimple-statement ((self java-constant-long-value) stream)
  "RETURNS the string representation that is used in the Jimple intermediate language of a java-constant-long-value .
This has the form: <number> 'L'"
  (declare (type stream stream))
  (call-next-method)
  (write-char #\L stream))

;;; CL: (progn (setq *print-escape* t) (with-open-file (s (merge-pathnames "test.txt") :direction :output) (write-sequence "\\Test" s)))
;;; results in \Test 

(defmethod write-jimple-statement ((self java-constant-string-value) stream)
  "RETURNS the string representation that is used in the Jimple intermediate language of a java-constant-string-value .
This has the form: '#\"'<string-value>'#\"'"
;  (let ((print-escape-state *print-escape*))
    (declare (type stream stream))
    (write-char #\" stream)
    (write-string (value self) stream)
    (write-char #\" stream))

(defmethod write-jimple-statement ((self java-constant-class-reference) stream)
  "RETURNS the string representation that is used in the Jimple intermediate language of a java-constant-class-reference.
This has the form: 'class ' '#\"'<string-value>'#\"'"
  (declare (type stream stream))
  (write-sequence "class " stream)
  (write-char #\" stream)
  (write-sequence (value self) stream)
  (write-char #\" stream))