;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-contexts.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Programming-Language Contexts Available to Programs
;;;  in the Java Language, both statically at compile-time,
;;;  and dynamically at run-time.
;;;  Examples are: method signatures, types,
;;;  the reference to the current instance, etc.
;;;  Contexts are used to transform the program such, that the context
;;;  information becomes accessible at run-time.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sat Jun 20 14:40:46 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:30:21 2010 (+0100)
;;;           By: Christian Hofmann
;;; 
;;; Copyright (C) 2009, Christian Hofmann. All rights reserved.
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

(in-package :JANA.MX.JAVA)

(defclass java-classifier-context (jana-meta-level-context jana-compile-time-context)
  ((java-classifier-declaration
    :READER classifier-declaration
    :INITARG :classifier-declaration
    :TYPE java-classifier-declaration
    :DOCUMENTATION "A java-classifier-declaration")
   (classifier-level-scope
    :ACCESSOR classifier-level-scope
    :TYPE java-classifier-level-scope
    :DOCUMENTATION "The classifier-level-scope."))
  (:DOCUMENTATION "The static context associatied in Java with classifiers."))

(defun java-classifier-context (classifier-declaration project)
  "Constructor"
  (declare (type java-classifier-declaration classifier-declaration)
           (type java-project project))
  (let ((instance (make-instance 'java-classifier-context
                                 :classifier-declaration classifier-declaration)))
    (setf (classifier-level-scope instance)
          (java-classifier-level-scope classifier-declaration project))
    instance))

(defmethod classifier-signature ((self java-classifier-context))
  "RETURNS a constant string value containing the signature of the
classifier corresponding to the java-classifier-context SELF."
  (declare #.*standard-optimize-settings*)
  (let ((signature ""))
    (declare (type string signature))
    (setq signature
          (with-output-to-string (output-stream)
           (write-full-java-classifier-signature
            (classifier-declaration self)
            output-stream)))
    (java-constant-string-value signature)))


(defclass java-instance-context (jana-meta-level-context jana-run-time-context)
  ((jana-type
    :ACCESSOR jana-type
    :DOCUMENTATION "The type of the instance that provides the context.")
   (this-context-variable
    :ACCESSOR this-context-variable
    :TYPE jimple-variable
    :DOCUMENTATION "A local variable that is used to reference the current instance.")
   (this-class-context-variable
    :ACCESSOR this-class-context-variable
    :TYPE jimple-variable
    :DOCUMENTATION "A local variable that is used to reference the class of the current instance.")
   (instance-level-scope
    :ACCESSOR instance-level-scope
    :DOCUMENTATION "The instance-level-scope."))
  (:DOCUMENTATION "The context available at run-time, which is shared by a single instance of a class."))

(defun java-instance-context (classifier-declaration project)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type java-classifier-declaration classifier-declaration)
           (type java-project project))
  (let ((instance (make-instance 'java-instance-context))
        (jana-type
         (java-object-reference-type (signature classifier-declaration))))
    (setf (jana-type instance)
          jana-type)
    (setf (this-context-variable instance)
          (jimple-variable +this-context-variable-name+ jana-type))
    (setf (this-class-context-variable instance)
          (jimple-variable +this-class-context-variable-name+ jana-type))
    (setf (instance-level-scope instance)
          (java-instance-level-scope classifier-declaration project))
    instance))

(defmethod instance-variable ((self java-instance-context) field-name)
  "RETURNS a jimple-instance-variable for the field with name FIELD-NAME
that is defined in the java-instance-context SELF."
  (declare #.*standard-optimize-settings*
           (type string field-name))
  (jimple-instance-variable
   (variable-reference (this-context-variable self))
   (signature (jana-type self))
   (gethash field-name
            (field-declarations (instance-level-scope self)))))

(defmethod this-reference-value ((self java-instance-context))
  "RETURNS the jimple-reference-value-this for the type of the
java-instance-context SELF."
  (jimple-reference-value-this (jana-type self)))

(defclass java-method-context (jana-meta-level-context jana-compile-time-context)
  ((method-declaration
    :ACCESSOR method-declaration
    :INITARG :method-declaration
    :TYPE java-method-declaration
    :DOCUMENTATION "A Java method-declaration."))
  (:DOCUMENTATION "Allows to refer to the static-aspects of a method's signature."))

;;; chr: might want to include the closure context as part of the method context!
(defun java-method-context (method-declaration)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type java-method-declaration method-declaration))
  (make-instance 'java-method-context
                 :method-declaration method-declaration))

(defmethod method-signature ((self java-method-context))
  "RETURNS a constant string value containing the signature of the
method-declaration corresponding to the java-method-context SELF."
  (declare #.*standard-optimize-settings*)
  (let ((signature  (with-output-to-string (output-stream)
                     (write-full-java-method-signature (method-declaration self)
                                                       output-stream))))
    (declare (type string signature))
    (java-constant-string-value signature)))
