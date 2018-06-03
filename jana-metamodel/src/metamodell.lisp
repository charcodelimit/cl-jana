;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.metamodel; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        metamodell.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;;    Metamodell.
;;;
;;;    This is a CLOS based implementation of a generic metamodel
;;;    for modern programming languages
;;;
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Aug  4 13:39:05 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:29:29 2010 (+0100)
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

(in-package #:JANA.METAMODEL)

;; all subclasses of jana-signature have to implement this protocol!
(defgeneric qualified-name (self)
  (:DOCUMENTATION "RETURNS the qualified name of a signature."))

(defgeneric unqualified-name (self)
  (:DOCUMENTATION "RETURNS the unqualified name of a signature."))

;; all programming-language elements that can have names have to implement a signature method
(defgeneric signature (self)
  (:DOCUMENTATION "RETURNS the jana-signature of a named programming-language element."))

(defclass jana-signature ()
  ()
  (:DOCUMENTATION "The signature of a language element can be used
to identify the language element in a specific scope
ofthe programming language.
Subclasses are responsible for implementing programming language
specific properties of signatures."))

(defmethod qualified-name ((self jana-signature))
  (error "Subclass responsibility. ~%Please define a specialized method for class ~A."
         (class-name (class-of self))))

(defmethod unqualified-name ((self jana-signature))
  (error "Subclass responsibility. ~%Please define a specialized method for class ~A."
         (class-name (class-of self))))

		       
(defclass jana-named-element ()
    ((name
      :ACCESSOR signature
      :INITARG :signature
      :TYPE jana-signature
      :DOCUMENTATION "A unique name identifying this element"))
  (:DOCUMENTATION "Language elements that can be explicitly referred to by a name."))


(defclass jana-name (jana-signature)
   ((qualified-name
    :INITARG :name
    :INITFORM ""
    :TYPE string
    :DOCUMENTATION "The name by which a language element can be referred to in a local scope."))
    (:DOCUMENTATION "The most primitive signature,where the name is qualified
by the scope in which it was declared (syntactic scope), or in which it is used (dynamic-scope).
The qualified name is the same as the unqualified name, because it has to be disambiguated by the scope."))

(defun jana-name (unqualified-name-string)
  "Memoizing Constructor"
  (declare (type string unqualified-name-string))
  (mmake-jana-name
   :constructor (make-instance 'jana-name :name unqualified-name-string)
   :java-memoization-table *java-memoization-table*
   :keys (unqualified-name-string)))

(defmethod make-load-form ((self jana-name) &optional environment)
  (make-load-form-saving-slots self 
                               :slot-names '(qualified-name) 
                               :environment environment))

(declaim (inline qualified-name))
(defmethod qualified-name ((self jana-name))
  "RETURNS the string found in slot qualified-name of the
jana-name instance SELF.
Even though,  there is no distinction between  qualified names
and unqualified names at the level of jana-names;
the name is qualified by the scope of the element to which
this name belongs."
  (declare #.*standard-optimize-settings*)  
  (slot-value self 'qualified-name))

(declaim (inline unqualified-name))
(defmethod unqualified-name ((self jana-name))
  "RETURNS the string found in slot qualified-name of the
jana-name instance SELF.
Even though,  there is no distinction between  qualified names
and unqualified names at the level of jana-names;
the name is qualified by the scope of the element to which
this name belongs."
  (declare #.*standard-optimize-settings*)
  (slot-value self 'qualified-name))

(defmethod qualified-name ((self jana-named-element))
  "RETURNS the qualified-name for the signature of the
jana-named-element instance SELF."
  (declare #.*standard-optimize-settings*)
  (qualified-name (signature self)))

(defmethod unqualified-name ((self jana-named-element))
  "RETURNS the unqualified-name for the signature of the
jana-named-element instance SELF."
  (declare #.*standard-optimize-settings*)
  (unqualified-name (signature self)))

(defclass jana-abstract-module ()
    ((parent-modules
      :TYPE list
      :DOCUMENTATION "The direct parent package(s) containing this package.")
     (child-modules
      :TYPE list
      :DOCUMENTATION "The direct child packages declared in this package.")
     (type-declarations
      :TYPE list
      :DOCUMENTATION "Types declared in the module."))
  (:DOCUMENTATION "A language element that allows grouping of types and modules.
It declares types, which might be virtual. An example for virtual types are compilation
units in C.) "))

(defclass jana-imaginary-module (jana-abstract-module)
  ()
  (:DOCUMENTATION "A module that is implicit in the language. Examples are C-Files.
C-Files contain an imaginary type."))

(defclass jana-module (jana-abstract-module jana-named-element)
  ()
  (:DOCUMENTATION "A concrete module. Examples are: namespaces, packages, etc."))

(defclass jana-type-declaration (jana-named-element)
  ()
  (:DOCUMENTATION "The declaration of a user-defined type, e.g. a structure type."))

(defclass jana-reference-type-declaration (jana-type-declaration)
  ((routines
    :TYPE list
    :DOCUMENTATION "Methods or method declarations belonging to the reference type.")
   (generalization-relation
    :TYPE jana-generalization
    :DOCUMENTATION "A generalization relation between (at-least) two reference types.")
   (attributes
    :TYPE list
    :DOCUMENTATION "Variable declarations local to the reference-type or instances of it"))
  (:DOCUMENTATION "Declares a user-defined reference type, that is, a class."))

(defclass jana-generalization ()
  ((sub-types
    :TYPE list
    :DOCUMENTATION "An (optional) list of the sub-types.")
   (super-types
    :TYPE list
    :DOCUMENTATION "A (mandatory) list of lists that contain ALL super-types.
Each sub-list represents a (mandatory) chain of super-types starting from a direct-supertype
to the root of the type-hierarchy. It is also possible to just list the direct super type in the chain.
The whole chain is calculated on the fly when needed."))
  (:DOCUMENTATION "A generalization relation.
Making the generalization a first class object allows to deal with
different kinds of inheritance explicitly in the model
(multiple, single, specialization, implementation, ..).
The sub-types slot contains a list with all direct subtypes. The super-types slot
contains a list of lists, where each sub-list represents a type-chain that lists
types in ascending order from the direct super-type to the root of the type-hierarchy."))

(defclass jana-variable (jana-named-element)
  ((variable-declaration
    :DOCUMENTATION "The declaration of the variable.")
   (variable-reference
    :DOCUMENTATION "The reference used to refer to a variable."))
  (:DOCUMENTATION "A variable can be declared and referenced in the metamodel."))

(defmethod signature ((self jana-variable))
  "The signature of a jana-variable is the same as the signature of its variable declaration."
  (declare #.*standard-optimize-settings*)
  (signature (slot-value self 'variable-declaration)))
  

(defclass jana-variable-declaration (jana-named-element)
  ((jana-type
    :TYPE jana-type
    :DOCUMENTATION "The (optional) type of the declared variable."))
  (:DOCUMENTATION "A variable declaration reserves a name to which a value can be bound.
Initialization instructions must be treated as separate assignment instructions.
The same applies to constant values. Example: int i = 0; -> int i; i = 0;"))

(defclass jana-variable-reference ()
  ()
  (:DOCUMENTATION "The name or value that is used to refer to a variable in a program."))

(defclass jana-routine-declaration (jana-named-element)
  ((argument-types
    :TYPE list
    :DOCUMENTATION "The types of the arguments. Because arguments can be uniquely identified by their position (for example in stack-based VM implementations) argument names are not modeled explicitly.")
   (return-type
    :TYPE jana-type
    :DOCUMENTATION "The type returned by the declared routine.")
   (owner
    :TYPE jana-name
    :DOCUMENTATION "The signature of the type that contains this routine declaration."))
  (:DOCUMENTATION "A declaration of name, argument-types and return-type of a routine."))

(defclass jana-routine-implementation (jana-routine-declaration)
  ((body
    :TYPE jana-closure
    :DOCUMENTATION "A syntactic closure containing the instructions that implement the routines behavior."))
  (:DOCUMENTATION "A routine is a declaration with a body that describes the closure that binds the values of the declared argument types to names."))

(defclass jana-closure ()
  ((instructions
    :TYPE list
    :DOCUMENTATION "A list of instructions that is executed when the closure is evaluated.")
   (branch-targets
    :TYPE list
    :DOCUMENTATION "A SORTED! list of integer indices that correspond to positions in the instructions list,
that are target instructions of branch-instructions.")
   (local-variables
    :DOCUMENTATION "The variableswith a scope local to the closure.")
   (argument-types
    :DOCUMENTATION "The type of the arguments identified by positions,
through wich values are passed to the closure when it is evaluated.")
   (return-type
    :TYPE jana-type
    :DOCUMENTATION "The syntactic type that is returned when the closure terminates. In some languages a special imaginary
type has to be used to indicate in the metamodel's instances that no value is passed."))
  (:DOCUMENTATION "A syntactic closure. Unlike in pure lambda calculus,
return type and local variables are made explicit."))



