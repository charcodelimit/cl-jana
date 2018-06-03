;;; -*- Mode: Lisp; Package: jana.metamodel -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;;
;;;
;;;

(in-package #:JANA.METAMODEL)

(defclass jana-type ()
  ()
  (:DOCUMENTATION "An element corresponding to a type in the programming language."))

(defclass jana-imaginary-type (jana-type)
  ()
  (:DOCUMENTATION "Imaginary types are types that can be distinguished on the conceptual level, but are not existing in a programming language as such. Examples are: Java - boolean (int {0,1}) and C - String (char[0..n]) and void. Virtual types hold global variables, etc.
 * Examples are: JNullType, JEnvType, JNoType,...
Other subtypes of jana-imaginary-type are used for scoping, like C-Files do for functions.
Imaginary types are not named elements of the language, because they can not be referred-to in the language."))

(defclass jana-primitive-type (jana-type)
  ()
  (:DOCUMENTATION "Primitive types are built into the language and don't need to be declared. For example in Java: String, int, long, double, float, .... Not all primitive types can be referred to in the language. Example: ReturnValue and Reference are types in Java are primitive types, but they cannot be referred to by name in the language."))

(defclass jana-reference-type (jana-type jana-named-element)
  ()
  (:DOCUMENTATION "Super class for object and array types."))