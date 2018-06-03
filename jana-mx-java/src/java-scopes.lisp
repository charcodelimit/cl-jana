;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-scopes.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Thu Jun 18 14:28:40 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:30:16 2010 (+0100)
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

(defclass java-local-scope (jana-local-lexical-scope)
  ((variable-declarations
    :READER variable-declarations
    :INITARG :variable-declarations
    :TYPE hash-table
    :DOCUMENTATION "A hash-table mapping variable-names to variables declared in the scope."))
   (:DOCUMENTATION "A lexical scope whose local extend is determined by the specific declaration
of a java-programming language element."))

(defun init-field-declarations (object-scope field-declaration-type classifier-declaration project)
  "Collects all field declarations defined in the java-classifier CLASSIFIER-DECLARATION,
and all of its super-classes that are defined in the java-project PROJECT.
Only field declarations of type FIELD-DECLARATION-TYPE are collected." 
  (declare #.*standard-optimize-settings*
           (type (or java-classifier-level-scope java-instance-level-scope)
                 object-scope)
           (type symbol field-declaration-type)
           (type java-classifier-declaration classifier-declaration)
           (type java-project project))
  (let ((super-type-chain '())
        (super-class-chain (list classifier-declaration)))
    (declare (type list super-type-chain super-class-chain))
    ;; single-inheritance
    (setq super-type-chain
          (first (extends (extends-relation classifier-declaration)))) 
    ;; get classifier-declaration for the types in the super-classes list
    (loop :for super-class-type :in super-type-chain
          :do (push (java-class project
                                (qualified-name (signature super-class-type)))
                    super-class-chain))
    ;; traverse the super-class-chain and add all field-declarations to the hash-table
    (loop
        :for current-classifier :in super-class-chain
        :do
        (loop :for field :in (fields current-classifier)
              :do  (when (typep field field-declaration-type)
                     (setf (gethash (unqualified-name (signature field))
                                    (variable-declarations object-scope))
                           field)))))
  object-scope)

(defclass java-classifier-level-scope (java-class-local-scope)
  ((variable-declarations
    :READER field-declarations
    :INITARG :field-declarations
    :TYPE hash-table
    :DOCUMENTATION "A hash-table mapping variable-names to variables declared in the scope."))
  (:DOCUMENTATION "The classifier-level scope of the Java language, which consists of class variables."))

(defun java-classifier-level-scope (classifier-declaration project)
  "Constructor."
  (declare #.*standard-optimize-settings*
           (type java-classifier-declaration classifier-declaration)
           (type java-project project))
  (let ((instance  (make-instance 'java-instance-level-scope
                                  :field-declarations (make-value-weak-synchronous-hashtable
                                                       :size (length (fields classifier-declaration))
                                                       :test 'equal))))
    (init-field-declarations instance
                             'java-class-variable-declaration
                             classifier-declaration
                             project)))

(defmethod field-signature ((self java-classifier-level-scope) field-name)
  "RETURNS a constant string value containing the signature of the
field-declaration corresponding to the field name string FIELD-NAME."
  (declare #.*standard-optimize-settings*
           (type string field-name))
  (let ((signature ""))
    (declare (type string signature))
    (setq signature
          (with-output-to-string (output-stream)
           (write-full-java-field-signature
            (gethash field-name (field-declarations self))
            output-stream)))
    (java-constant-string-value signature)))
                                  

(defclass java-instance-level-scope (java-local-scope java-project)
  ((variable-declarations
    :READER field-declarations
    :INITARG :field-declarations    
    :TYPE hash-table
    :DOCUMENTATION "A hash-table mapping variable-names to variables declared in the scope.")
   (parent-scope
    :ACCESSOR parent-scope
    :TYPE java-class-level-scope
    :DOCUMENTATION "The parent class-level scope."))
  (:DOCUMENTATION "The scope assciated in Java with objects."))

(defun java-instance-level-scope (classifier-declaration project)
  "Constructor. Fields declared in superclasses are not considered!"
  (declare (type java-classifier-declaration classifier-declaration)
           (type java-project project))
  (let ((instance  (make-instance 'java-instance-level-scope
                                  :field-declarations (make-value-weak-synchronous-hashtable
                                                       :size (length (fields classifier-declaration))
                                                       :test 'equal))))
    (init-field-declarations instance
                             'java-class-variable-declaration
                             classifier-declaration
                             project)
      instance))

(defclass java-method-level-scope (java-local-scope)
  ((parent-scope
    :ACCESSOR parent-scope
    :TYPE java-instance-level-scope
    :DOCUMENTATION "The parent instance-level scope."))
  (:DOCUMENTATION "The scope assciated in Java with methods."))

(defun java-method-level-scope (jimple-closure)
  (local-variables jimple-closure))