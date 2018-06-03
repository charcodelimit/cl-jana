;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-values.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;;  Metamodel entities that model values in the Java language.
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Thu Aug  7 22:57:41 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:09 2010 (+0100)
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

(in-package :JANA.JAVA)

(defclass java-value (jana-value)
  ((jana-type
    :ACCESSOR jana-type
    :TYPE jana-type
    :INITARG :jana-type))
  (:DOCUMENTATION "A Java value."))

(defclass java-null-value (jana-null-value java-value)
  ()
  (:DOCUMENTATION "The value 'null'"))

(defun java-null-value ()
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*)  
  (let ((unique-id 'java-null-value))
    (mmake-java-value
     :constructor (make-instance 'java-null-value
				 :jana-type (java-null-type))
     :java-memoization-table *java-memoization-table*
     :keys (unique-id))))


(defclass java-constant-value (jana-constant-value java-value)
  ((value
    :ACCESSOR value
    :INITARG :value
    :DOCUMENTATION "The constant's value"))
  (:DOCUMENTATION "A Constant Value."))

;;;
;;; (defclass test-thing () ((value :accessor value :initarg :value)))
;;; (create-constant-value-constructor 'test-thing)
;;; (assert (eql (value (test-thing 12)) 12))
;;; (describe #'test-thing)
;;; (defun test-thing () nil)
;;; (create-constant-value-constructors '(test-thing))
;;; (assert (eql (value (test-thing 12)) 12))
;;; (describe #'test-thing)
;;;
(defmacro create-constant-value-constructors (classname-list)
  "takes as argument a quoted list of classname symbols"
  (let ((l nil))
    (dolist (classname (eval classname-list))
      (push
       `(defun ,classname (value)
         ,(format nil "Constructor generated for class ~S" classname)
         (declare #.*standard-optimize-settings*)
	 (mmake-java-value
	  :constructor (make-instance ',classname :value value)
	  :java-memoization-table *java-memoization-table*
	  :keys (',classname value)))
       l))
    (setq l (nreverse l))
    (append '(mapcar #'eval) (list (append '(quote) (list l))))))

(defmacro create-constant-value-constructor (classname)
  "takes as argument the quoted class-name"
  `(defun ,(eval classname) (value)
    ,(format nil "Constructor generated for class ~S" (eval classname))
    (declare #.*standard-optimize-settings*)
    (mmake-java-value
	 :constructor (make-instance ',classname :value value)
	 :java-memoization-table *java-memoization-table*
	 :keys (',classname value))
    ))

;;;

(defclass java-constant-fp-value (java-constant-value)
  ((value
    :TYPE float)
   (out-of-range-value
    :ACCESSOR out-of-range-value
    :INITARG :out-of-range-value
    :TYPE string
    :DOCUMENTATION "Java supports values that may be out of range for some Common Lisp Implementations like 2.2250738585072014E-308. Furthermore do we need to represent the values -infinity +infinity and NaN. Therefore an out-of-range slot exists where these values are stored as Strings."))
  (:DOCUMENTATION "Abstract Class for constant floating point values."))

(defclass java-constant-double-value (java-constant-fp-value)
  ((jana-type
    :INITFORM (make-java-basic-type 'double)))
  (:DOCUMENTATION "An IEEE floating point value with double precision."))

(defun java-constant-double-value (value)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*)
  (cond ((stringp value)
	 (mmake-java-value
	  :constructor (make-instance 'java-constant-double-value
				      :out-of-range-value value)
	  :java-memoization-table *java-memoization-table*
	  :keys ('java-constant-double-value value)))
	((floatp value)
	 (mmake-java-value
	  :constructor (make-instance 'java-constant-double-value
				      :value value)
	  :java-memoization-table *java-memoization-table*
	  :keys ('java-constant-double-value value)))
	(t
	 (error "Trying to create a java-constant-double value from a
 value with a type different from string or float! ~A" value))))

(defclass java-constant-exponent-free-double-value (java-constant-double-value)
  ()
  (:DOCUMENTATION "An IEEE Floating point value with double precision,
whose representation contains no exponent."))

(defun java-constant-exponent-free-double-value (value)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*)
  (cond ((stringp value)
	 (mmake-java-value
	  :constructor (make-instance 'java-constant-exponent-free-double-value
				      :out-of-range-value value)
	  :java-memoization-table *java-memoization-table*
	  :keys ('java-constant-double-value value)))
	((floatp value)
	 (mmake-java-value
	  :constructor (make-instance 'java-constant-exponent-free-double-value
				      :value value)
	  :java-memoization-table *java-memoization-table*
	  :keys ('java-constant-double-value value)))
	(t
	 (error "Trying to create a java-constant-exponent-free-double
value from a value with a type different from string or float! ~A" value))))

(defclass java-constant-float-value (java-constant-fp-value)
  ((jana-type
    :INITFORM (make-java-basic-type 'float))))

(defun java-constant-float-value (value)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*)  
  (cond ((stringp value)
	 (mmake-java-value
	  :constructor (make-instance 'java-constant-float-value
				      :out-of-range-value value)
	  :java-memoization-table *java-memoization-table*
	  :keys ('java-constant-float-value value)))
	((floatp value)
	 (mmake-java-value
	  :constructor (make-instance 'java-constant-float-value
				      :value value)
	  :java-memoization-table *java-memoization-table*
	  :keys ('java-constant-float-value value)))
	(t
	 (error "Trying to create a java-constant-float value from a
 value with a type different from string or float! ~A" value))))

;;;

(defclass java-constant-integral-value (java-constant-value)
  ()
  (:DOCUMENTATION "Abstract Class"))

(defclass java-constant-boolean-value (java-constant-integral-value)
  ((value
    :TYPE (integer 0 1))
   (jana-type
    :INITFORM (make-java-basic-type 'boolean))))

(defclass java-constant-byte-value (java-constant-integral-value)
  ((value
    :TYPE (signed-byte 8))
   (jana-type
    :INITFORM (make-java-basic-type 'byte))))

(defclass java-constant-short-value (java-constant-integral-value)
  ((value
    :TYPE (signed-byte 16))
   (jana-type
    :INITFORM (make-java-basic-type 'short))))

(defclass java-constant-int-value (java-constant-integral-value)
  ((value
    :TYPE (signed-byte 32))
   (jana-type
    :INITFORM (make-java-basic-type 'int))))

(defclass java-constant-long-value (java-constant-integral-value)
  ((value
    :TYPE (signed-byte 64))
   (jana-type
    :INITFORM (make-java-basic-type 'long))))

(defclass java-constant-char-value (java-constant-integral-value)
  ((value
    :TYPE (unsigned-byte 16))
   (jana-type
    :INITFORM (make-java-basic-type 'char))))

;; generate constructors
(create-constant-value-constructors
 '(java-constant-byte-value java-constant-short-value java-constant-int-value
   java-constant-long-value java-constant-char-value))
;;;

(defun java-constant-boolean-value (boolean-value)
  "Creates a java-constant boolean value using
the generalized-boolean BOOLEAN-VALUE.
RETURNS the corresponding java-constant-int value."
  (if boolean-value
      (make-instance 'java-constant-boolean-value :value 1)
      (make-instance 'java-constant-boolean-value :value 0)))

(defclass java-constant-string-value (java-constant-value)
  ((value
    :TYPE string))
  (:DOCUMENTATION "A String literal representing the value of type java.lang.String.
No distinction is actually made between UTF8 encoded constant string values and the String references
used in the Java Bytecode to point to this constant.
If such a distinction is needed, subclasses should be used to implement this specialized semantics accordingly."))

;; does not use memoization, as memoization overhead is too high for long strings
(defun java-constant-string-value (value
                                   &optional (type (java-object-reference-type (java-signature +string-class-signature+))))
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type string value)
           (type jana-type type))
  (make-instance 'java-constant-string-value
		 :value value
		 :jana-type type))

(defclass java-constant-class-reference (java-constant-value)
  ()
  (:DOCUMENTATION "Symbolic references to classes are represented as Strings
of their fully qualified names"))   

(defun java-constant-class-reference (value
                                      &optional (type (java-object-reference-type (java-signature +class-class-signature+))))
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type jana-type type)
           (type string value))
  ;; make sure that the class-name is in binary form
  (let ((signature (substitute #\/ #\. value)))
    (declare (type string signature))
    (mmake-java-value
     :constructor (make-instance 'java-constant-class-reference
                                 :value signature
                                 :jana-type type)
     :java-memoization-table *java-memoization-table*
     :keys (signature))))

(defun java-constant-value (lisp-value)
  "Constructor.
Creates a java-constant-value that is compatible with the passed lisp-value LISP-VALUE.
For details of the conversion scheme take a look at the function's sourcecode."
  (cond
    ((typep lisp-value 'string)
     (java-constant-string-value lisp-value))    
    ((typep lisp-value '(signed-byte 8))
     (java-constant-byte-value lisp-value))
    ((typep lisp-value '(unsigned-byte 16))
     (java-constant-char-value lisp-value))
    ((typep lisp-value '(signed-byte 16))
     (java-constant-short-value lisp-value))
    ((typep lisp-value '(signed-byte 32))
     (java-constant-int-value lisp-value))
    ((typep lisp-value '(signed-byte 64))
     (java-constant-long-value lisp-value))
    ((typep lisp-value 'boolean)
     (java-constant-boolean-value lisp-value))
    ((typep lisp-value 'single-float)
     (java-constant-float-value lisp-value))
    ((typep lisp-value 'double-float)
     (java-constant-double-value lisp-value))
    (t
     (error "Cannot create a constant value from a lisp-value of type ~A"
            (type-of lisp-value)))))