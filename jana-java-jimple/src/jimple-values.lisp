;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-values.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;;    Metamodel for the Jimple Intermediate Representation of the Java Language.
;;;    Representation of Values.
;;;
;;;    This is a CLOS based implementation of the representation of values
;;;    in the Jimple Intermediate Language for Java. 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Aug  8 23:19:00 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:24:40 2010 (+0100)
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

(in-package :JANA.JAVA.JIMPLE)

;;; -------

(defclass jimple-reference-value (java-value)
  ()
  (:DOCUMENTATION "A value that is obtained through a reference."))

;;; 

(defclass jimple-reference-value-local (jimple-reference-value)
  ((variable-name
    :READER variable-name
    :INITARG :variable-name
    :TYPE string
    :DOCUMENTATION "The Signature by which the variable is referred to."))
   (:DOCUMENTATION "A reference to the value of a local variable.
The variable names are only used in the Jimple intermediate representation.
Local variables become implicit stack locations or references to the local variable table when they are compiled into Java Bytecodes.  
USE the ENV to create only one instance of a local variable per scope!"))

(defmethod unqualified-name ((self jimple-reference-value-local))
  "RETURNS the variable name of the jimple-reference-value SELF."
  (variable-name self))

(defun jimple-reference-value-local (variable-name variable-type)
  "Memoizing Constructor"
  (mmake-java-value 
   :constructor (make-instance 'jimple-reference-value-local
			       :variable-name variable-name
			       :jana-type variable-type)
   :java-memoization-table *java-memoization-table*
   :keys (variable-name variable-type)))

(defmethod jimple-reference-value-from-declaration ((local-variable-declaration java-local-variable-declaration))
  "RETURNS a reference to a local-variable using the local-variable declaration
LOCAL-VARIABLE-DECLARATION."
  (jimple-reference-value-local (qualified-name local-variable-declaration)
                                (jana-type local-variable-declaration)))

;;;

(defclass jimple-reference-value-field (jimple-reference-value)
  ((field-name
    :ACCESSOR field-name
    :INITARG :field-name
    :TYPE string
    :DOCUMENTATION "The name of the referenced field.")
   (classifier-signature
    :READER field-owner
    :INITARG :field-owner
    :TYPE java-signature
    :DOCUMENTATION "The signature of the classifier in whose scope the field is looked up."))
  (:DOCUMENTATION "The reference to the value of a field."))

(defmethod reference-values-equal ((self jimple-reference-value-field) (other jimple-reference-value-field))
  "Compares two jimple-reference-value field references SELF and OTHER.
If the field-name and field-owner match, T is returned."
  (and (equal (field-name self)
              (field-name other))
       (equal (field-owner self)
              (field-owner other))))

(defmethod reference-values-equal ((self t) (other jimple-reference-value-field))
  nil)

(defmethod reference-values-equal ((self jimple-reference-value-field) (other t))
  nil)

(defmethod unqualified-name ((self jimple-reference-value-field))
  "RETURNS the variable name of the jimple-reference-value SELF."
  (field-name self))

(defclass jimple-reference-value-instance-variable (jimple-reference-value-field)
  ((local-variable-instance-reference
    :READER local-variable-instance-reference
    :TYPE jimple-reference-value-local
    :INITARG :local-variable-instance-reference
    :DOCUMENTATION "A Variable containing the reference to the instance where the instance-variable belongs to."))
  (:DOCUMENTATION "A reference to the value of an instance-variable."))

(defun jimple-reference-value-instance-variable (local-variable-instance-reference classifier-signature type field-name)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor
   (make-instance 'jimple-reference-value-instance-variable
		  :local-variable-instance-reference local-variable-instance-reference  
		  :field-owner classifier-signature
		  :jana-type type
		  :field-name field-name)
   :java-memoization-table *java-memoization-table*
   :keys (local-variable-instance-reference classifier-signature field-name)))


(defclass jimple-reference-value-class-variable (jimple-reference-value-field)
  ()
  (:DOCUMENTATION "A reference to the value of a class-variable."))

(defun jimple-reference-value-class-variable (classifier-signature type field-name)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor (make-instance 'jimple-reference-value-class-variable
			       :field-owner classifier-signature
			       :jana-type type
			       :field-name field-name)
  :java-memoization-table *java-memoization-table*
  :keys (classifier-signature field-name)))


(defclass jimple-reference-value-array (jimple-reference-value)
  ((local-variable-instance-reference
    :READER local-variable-instance-reference
    :INITARG :local-variable-instance-reference
    :TYPE jimple-reference-value-local
    :DOCUMENTATION "A Variable containing the reference to the array whose element is referenced.")
   (index
    :READER index
    :INITARG :index
    :DOCUMENTATION "A variable or integer constant denoting the indexed element."))
  (:DOCUMENTATION "A reference to the value of an array element."))

(defun jimple-reference-value-array (local-variable type index)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor (make-instance 'jimple-reference-value-array
			       :local-variable-instance-reference local-variable
			       :jana-type type
			       :index index)
   :java-memoization-table *java-memoization-table*
   :keys (local-variable type index)))

;;;

(defclass jimple-reference-value-this (jimple-reference-value)
  ()
  (:DOCUMENTATION "A reference to the instance in whose scope the reference resides."))

(defun jimple-reference-value-this (type)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor (make-instance 'jimple-reference-value-this
			       :jana-type type)
   :java-memoization-table *java-memoization-table*
   :keys ('this type)))

(defmethod reference-values-equal ((self jimple-reference-value-this) (other jimple-reference-value-this))
  (equal (signature (jana-type self)) (signature (jana-type other))))

(defmethod reference-values-equal ((self t) (other jimple-reference-value-this))
  nil)

(defmethod reference-values-equal ((self jimple-reference-value-this) (other t))
  nil)




(defclass jimple-reference-value-argument (jimple-reference-value)
  ((index
    :READER index
    :INITARG :index
    :DOCUMENTATION "The index denoting the position of a method's argument"))
  (:DOCUMENTATION "A reference to the value passed through a method's argument."))
  
(defun jimple-reference-value-argument (argument-index argument-type)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor (make-instance 'jimple-reference-value-argument
			       :index argument-index
			       :jana-type argument-type)
   :java-memoization-table *java-memoization-table*
   :keys (argument-type argument-index)))

(defclass jimple-reference-value-caught-exception (jimple-reference-value)
  ()
  (:DOCUMENTATION "A reference to a caught exception.
Example: catch(Exception e)"))

(defun jimple-reference-value-caught-exception (type)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor (make-instance 'jimple-reference-value-caught-exception
			       :jana-type type)
   :java-memoization-table *java-memoization-table*
   :keys ('caughtexception type)))


(defclass jimple-reference-value-method (jimple-reference-value)
  ((method-name
    :READER method-name
    :INITARG :method-name
    :TYPE string
    :DOCUMENTATION "The name of the referenced method.")
   (argument-types
    :READER argument-types
    :INITARG :argument-types
    :TYPE list
    :DOCUMENTATION "The argument types of the referenced method."))
  (:DOCUMENTATION "A reference to a method.
Can be thought of as a closure."))

(defun jimple-reference-value-method (method-name return-type argument-types)
  "Memoizing Constructor"
  (mmake-java-value
   :constructor (make-instance 'jimple-reference-value-method
			       :method-name method-name
			       :argument-types argument-types
			       :jana-type return-type)
   :java-memoization-table *java-memoization-table*
   :keys (method-name return-type argument-types)))

(defmethod jimple-reference-value-from-declaration ((method-declaration java-method-declaration))
  "RETURNS a reference to a method using the java-method-declaration
METHOD-DECLARATION."
  (jimple-reference-value-method (qualified-name method-declaration)
                                 (return-type method-declaration)
                                 (argument-types method-declaration)))
