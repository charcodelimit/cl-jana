;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-types.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Aug  6 14:51:35 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:12 2010 (+0100)
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

(in-package #:JANA.JAVA)

(defgeneric java-type-signature (self)
  (:DOCUMENTATION "RETURNS a string denoting the signature that is used to indicate this type in a Java program."))

(defclass java-type ()
  ()
  (:DOCUMENTATION "Mixin Class. All Java types have a signature that uniquely identifies them.
 Therefore, they are named elements."))

(defclass java-reference-type (jana-reference-type java-type)
  ()
  (:DOCUMENTATION "Java reference types are the object-type (which includes class, interface and enum), and the array-type"))

(defmethod java-package-name ((self java-reference-type))
  (declare #.*standard-optimize-settings*)  
  (previous-signature-element (signature self)))

(defclass java-null-type (java-reference-type)
  ()
  (:DOCUMENTATION "The type of the expression null, which has no name. 
'The null reference is the only possible value of an expression of null type and can always be converted to any reference type. In practice, the programmer can ignore the null type and just pretend that null is a special literal that can be of any reference type.' from: The JVM Specification 2nd Edition"))

(defun java-null-type ()
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*)
  (let ((unique-id 'java-null-type))
    (mmake-java-type
     :constructor (make-java-null-type)
     :java-memoization-table *java-memoization-table*
     ;; chr: workaround a bug in CLISP 2.47 (2008-10-23) (built 3434708170)
     :keys (unique-id))))

#.(declaim (inline make-java-null-type))
(defun make-java-null-type ()
  "Constructor"
  (declare #.*standard-optimize-settings*)
  (make-instance 'java-null-type :signature (jana-name "null_type")))

(defclass java-object-reference-type (java-reference-type)
  ()
  (:DOCUMENTATION "includes class, interface and enum."))

(defun java-object-reference-type (signature)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type java-signature signature))
  (mmake-java-type
   :constructor (make-java-object-reference-type signature)
   :java-memoization-table *java-memoization-table*
   :keys (signature)))

(defun make-java-object-reference-type (signature)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type java-signature signature))
  (make-instance 'java-object-reference-type
                 :signature signature))

(defmethod make-load-form ((self java-object-reference-type) &optional environment)
  (make-load-form-saving-slots self 
                               :slot-names '(name) 
                               :environment environment))  


(defclass java-array-type (java-reference-type)
  ((dimensions
    :READER dimensions
    :INITARG :dimensions
    :TYPE fixnum
    :DOCUMENTATION "The array dimensions.")
   (element-type
    :READER element-type
    :INITARG :element-type
    :DOCUMENTATION "The basic type of the array elements."))
  (:DOCUMENTATION "An array type can have elements of any valid Java type."))

(defun array-type-signature (element-type dimensions)
  "RETURNS the array-type-signature based on the
generic method QUALIFIED-NAME"
  (declare #.*standard-optimize-settings*
           (type java-type element-type)
           (type fixnum dimensions))
  (when (debug-mode)
    (assert (or (typep element-type 'java-primitive-type)
		(typep element-type 'java-reference-type))))
  (let ((element-type-signature (signature element-type))
	(dimension-suffix ""))
    (declare (type jana-name element-type-signature))
    (when (debug-mode)
      (format t "~% ~A/~A"
	      (unqualified-name element-type-signature)
	      (qualified-name element-type-signature)))
    (dotimes (times dimensions)
      (setq dimension-suffix (concatenate 'string dimension-suffix "[]")))
    (jana-name (concatenate 'string
			    (qualified-name element-type-signature)
			    dimension-suffix))))

(defun java-array-type (dimensions element-type)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type java-type element-type)
           (type fixnum dimensions))  
  (mmake-java-type
   :constructor (make-java-array-type dimensions element-type)
   :java-memoization-table *java-memoization-table*
   :keys (element-type dimensions)))

(defun make-java-array-type (dimensions element-type)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type java-type element-type)
           (type fixnum dimensions))
  (when (debug-mode)
    (format t "~%Constructor java-array-type"))
  (when (< dimensions 1)
    (error "Trying to Construct a Java Array Type with dimensions < 1! [~S]" dimensions))
  (let ((array-type-signature))
    (setq array-type-signature
	  (array-type-signature element-type dimensions))
    (when (debug-mode)
      (format t " ~A" (qualified-name array-type-signature)))
    (make-instance 'java-array-type
		   :signature array-type-signature
		   :dimensions dimensions
		   :element-type element-type)))
		    
(defclass java-primitive-type (jana-primitive-type
			       java-type jana-named-element)
  ((name
    :ACCESSOR signature
    :TYPE java-signature))
  (:DOCUMENTATION "The Java Basic Types"))

(defmethod java-type-signature ((self java-primitive-type))
  (declare #.*standard-optimize-settings*)
  (qualified-name (signature self)))

(defclass java-void-type (jana-imaginary-type java-primitive-type)
; specialize jana-reference-type
  ((name
    :INITFORM (java-signature "void")))
  (:DOCUMENTATION
"This type signals that a method returns no value.
Thus, it is only used to represents a method-return-type."))

(defclass java-numeric-type (java-primitive-type)
  ()
  (:DOCUMENTATION "Java's numeric types (float, double, int, short, char, long, byte)"))

(defclass java-floating-point-type (java-numeric-type)
  ()
  (:DOCUMENTATION "Java's IEEE 754 standard conform float and double types."))

(defclass java-float (java-floating-point-type)
  ((name
    :INITFORM  (java-signature "float")))
  (:DOCUMENTATION "Parameter, float, float-extended-exponent
N 	24 	24 	
K 	8 	11 	
Emax 	+127 	+1023 	
Emin 	-126 	-1022"))

(defclass java-double (java-floating-point-type)
  ((name
    :INITFORM (java-signature "double")))
  (:DOCUMENTATION "Parameter, double, double-extended-exponent
N 	53	53
K 	11      15 	 	
Emax 	+1023 	 +16383 	
Emin 	-1022 	 -16382"))

(defclass java-integral-type (java-numeric-type)
  ()
  (:DOCUMENTATION "Java's signed two's-complement integers and the 16-bit unsigned integers that represent
Unicode characters."))

(defclass java-byte (java-integral-type)
  ((name
    :INITFORM (java-signature "byte")))
  (:DOCUMENTATION "8-bit two's complement integer"))

(defclass java-short (java-integral-type)
  ((name
    :INITFORM (java-signature "short")))
  (:DOCUMENTATION "16-bit two's complement integer"))

(defclass java-int (java-integral-type)
  ((name
    :INITFORM (java-signature "int")))
  (:DOCUMENTATION "32-bit two's complement integer"))

(defclass java-long (java-integral-type)
  ((name
    :INITFORM (java-signature "long")))
  (:DOCUMENTATION "64-bit two's complement integer"))

(defclass java-char (java-integral-type)
  ((name
    :INITFORM (java-signature "char")))
  (:DOCUMENTATION "16-bit unsigned integer that represents Unicode characters"))

(defclass java-boolean-type (jana-imaginary-type java-int)
  ((name
    :INITFORM (java-signature "boolean")))
  (:DOCUMENTATION
"An integer value where any nonzero value (value != 0) represents true."))

(defun java-basic-type (type-symbol)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type symbol type-symbol))
  (mmake-java-type
   :constructor (make-java-basic-type type-symbol)
   :java-memoization-table *java-memoization-table*
   :keys (type-symbol)))

(defun make-java-basic-type (type-symbol)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type symbol type-symbol))
  (let ((basic-type-class-name))
    (setq basic-type-class-name
	  (cond
	    ((eql type-symbol 'void) 'java-void-type)
	    ((eql type-symbol 'boolean) 'java-boolean-type)
	    ((eql type-symbol 'float) 'java-float)
	    ((eql type-symbol 'double) 'java-double)
	    ((eql type-symbol 'byte) 'java-byte)
	    ((eql type-symbol 'short) 'java-short)
	    ((eql type-symbol 'int) 'java-int)
	    ((eql type-symbol 'long) 'java-long)
	    ((eql type-symbol 'char) 'java-char)
	    (t (error "Unknown java-basic-type found: ~S" type-symbol))))
    (make-instance basic-type-class-name)))

(defun determine-object-type (type-string)
  (declare #.*standard-optimize-settings*
           (type string type-string))
  (let ((match (mismatch type-string "[]" :from-end t))
        (pos 0)
        (oldpos (length type-string))
        (dimensions 0))
    (declare (type fixnum pos oldpos dimensions))
    (if match
        (setq pos match)
        (setq pos 0))
    (loop
        :until (= pos oldpos)
        :do (setq oldpos pos)
            (setq match (mismatch type-string "[]" :end1 oldpos :from-end t))
            (if match
                (setq pos match)
                (setq pos 0))
            (format t "~A " pos)
            (incf dimensions))
    (values (subseq type-string 0 pos)
            dimensions)))

(defmethod java-type ((self string))
  "Creates the java-type corresponding to the string SELF."
  (declare #.*standard-optimize-settings*)
  (cond
    ((find-symbol (string-upcase self) :jana.java)
     (make-java-basic-type
      (find-symbol (string-upcase self)
                   :jana.java)))
    (t
     (multiple-value-bind (qualified-name dimensions)
         (determine-object-type self)
       (if (> 0 dimensions)
           (java-array-type
            dimensions
            (java-object-reference-type (java-signature qualified-name)))
           (java-object-reference-type
            (java-signature qualified-name)))))))