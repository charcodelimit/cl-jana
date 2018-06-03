;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        java-metamodell.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Java Language.
;;;
;;;    This is a CLOS based implementation of a metamodel
;;;    for Java
;;;
;;;
;;; TODO
;;;   add a hash-table that maps java-types to initial values.
;;;   initialize initial values for field-declarations look-up with:
;;;   (class-name (class-of FIELD-TYPE)) 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Aug  4 16:47:37 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:17 2010 (+0100)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric valid-modifiers (self)
  (:DOCUMENTATION "RETURNS a list of modifier symbols, that are valid for the metamodel-element SELF."))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defclass java-classpath ()
;  ((file-names
;    :ACCESSOR file-names
;    :INITARG :file-names
;    :DOCUMENTATION "A list of filenames that constitude the classpath"))
;  (:DOCUMENTATION "The Java Classpath, where packages and classes are looked up."))

;(defun java-classpath (classpath-elements)
;  "Constructor"
;  (make-instance 'java-classpath
;		 :file-names classpath-elements))

;------------------------------------

(defclass java-signature (jana-name)
  ((qualified-name
    :ACCESSOR qualified-name
    :INITARG :qualified-name
    :TYPE string
    :DOCUMENTATION "The fully qualified name, which includes the full package name."))
  (:DOCUMENTATION "A name string that uniquely identifies a Java language element,
like a class."))

(defun java-signature (qualified-java-name)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type string qualified-java-name))
  (mmake-java-signature
   :constructor (make-instance 'java-signature
			       :qualified-name qualified-java-name)
   :java-memoization-table *java-memoization-table*
   :keys (qualified-java-name)))
;(lookup-java-signature *java-memoization-table* qualified-name))
;(load "workspace")
;Real time: 1.384423 sec.
;Run time: 1.384087 sec.
;Space: 88740224 Bytes
  
(defun make-java-signature (qualified-name)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type string qualified-name))
  (make-instance 'java-signature :qualified-name qualified-name))

;;

(defmethod unqualified-name ((self java-signature))
  "RETURNS the unqualified name of a java-signature by
determining the last signature element of the qualified-name."
  (declare #.*standard-optimize-settings*)
  (last-signature-string-element (qualified-name self)))

(defmethod file-name ((self java-signature))
  "RETURNS the file name without the filename extension.
The file-name is determined by converting the qualified-name."
  (declare #.*standard-optimize-settings*)
  (substitute +FILE-SEPARATOR-CHAR+ +SIGNATURE-ELEMENT-SEPARATOR+
	      (qualified-name self)))

;;

(defmethod previous-signature-element ((self java-signature))
  (declare #.*standard-optimize-settings*)
  (let ((last-separator-position
	 (position +SIGNATURE-ELEMENT-SEPARATOR+ (qualified-name self) :from-end t)))
    (if last-separator-position
	(subseq (qualified-name self) 0 last-separator-position)
	"")))

(defun last-signature-string-element (signature-string)
  "RETURNS the last element in a signature String that has been preceded by the
+SIGNATURE-ELEMENT-SEPARATOR+"
  (declare #.*standard-optimize-settings*)  
  (let ((last-separator-position
	 (position +SIGNATURE-ELEMENT-SEPARATOR+ signature-string :from-end t)))
    (if last-separator-position
	(subseq signature-string (+ last-separator-position 1) (length signature-string))
	signature-string)))

;------------------------------------

(defclass java-package (jana-module)
  ((parent-modules
    :ACCESSOR parent-package
    :INITFORM ())
   (child-modules
    :ACCESSOR child-packages
    :INITFORM ())
   (type-declarations
    :ACCESSOR child-classifiers
    :INITFORM ()))
  (:DOCUMENTATION "A Java package."))

(defun java-package (fully-qualified-name)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type string fully-qualified-name))
  (mmake-java-package
   :constructor (make-instance 'java-package
		 :signature (java-signature fully-qualified-name))
   :java-memoization-table *java-memoization-table*
   :keys (fully-qualified-name)))
    
(defmethod qualified-name ((self java-package))
  "RETURNS the qualified name of the java-package"
  (declare #.*standard-optimize-settings*)
  (qualified-name (signature self)))
  
;------------------------------------

(defclass java-classifier-declaration (jana-reference-type-declaration)
  ((routines
    :ACCESSOR methods
    :INITFORM ())
   (generalization-relation
    :TYPE java-extends
    :ACCESSOR extends-relation)
   (implementation-relation
    :TYPE java-implements
    :ACCESSOR implements-relation
    :DOCUMENTATION "The implementation relation denotes the interfaces implemented by this classifier.")
   (attributes
    :ACCESSOR fields
    :INITFORM ())
   (parent-package
    :ACCESSOR parent-package
    :TYPE jana-abstract-module
    :DOCUMENTATION "The package in which the reference-type definition resides.")
   (source-file
    :ACCESSOR source-file-name
    :TYPE string
    :DOCUMENTATION "The name of the source-file where the  source-code of the classifier can be found.")
   (class-annotations
    :ACCESSOR class-annotations
    :TYPE list
    :DOCUMENTATION "A list of annotations. Annotations are special modifiers that are declared
by a java annotation type.")
   (class-modifiers
    :ACCESSOR class-modifiers
    :DOCUMENTATION "Modifiers for the reference-type.
Since there are no Interface or Enum specific modifiers, the slot is named class-modifiers.")
   (nested-classifiers
    :ACCESSOR nested-classifiers
    :INITFORM ()
    :TYPE list
    :DOCUMENTATION "A list of reference-types that are declared by this reference-type."))
  (:DOCUMENTATION "Declares a Java reference-type like Class, Interface, or Enum."))

(declaim (inline java-classifier-declaration))
(defun java-classifier-declaration (classifier-type parent-package
				    source-file annotations modifiers signature
				    extends-relation implements-relation
				    nested-classifiers fields methods)
  "Protected Memoizing Constructor"
  (declare #.*standard-optimize-settings*)  
  (mmake-java-classifier-declaration
   :constructor
   (make-java-classifier-declaration classifier-type parent-package
                                     source-file annotations modifiers
                                     signature
                                     extends-relation implements-relation
                                     nested-classifiers fields methods)
   :java-memoization-table *java-memoization-table*
   :keys (signature)))
   
(defun make-java-classifier-declaration (classifier-type parent-package 
                                         source-file annotations modifiers signature
					 extends-relation implements-relation
					 nested-classifiers fields methods)
    "Protected Constructor"
    (declare #.*standard-optimize-settings*)
    (when (verbose-mode)
      (format t "~% ~A ~A " classifier-type (qualified-name signature)))
    (let ((instance))
      (setq instance (make-instance classifier-type :signature signature))
      ;; initialization
      (push instance (child-classifiers parent-package)) ; chr: indirect lookup, could be replaced by a direct reference
      (setf (parent-package instance) parent-package)
      (setf (source-file-name instance) source-file)
      (setf (class-annotations instance) annotations)
      (setf (class-modifiers instance) modifiers)
      (setf (extends-relation instance) extends-relation)
      (setf (implements-relation instance) implements-relation)
      (setf (nested-classifiers instance) nested-classifiers)
      (setf (fields instance) fields)
      (setf (methods instance) methods)
      instance))

(defmethod qualified-name ((self java-classifier-declaration))
  (declare #.*standard-optimize-settings*)
  (qualified-name (signature self)))

(defmethod super-types ((self java-classifier-declaration))
  "RETURNS the types of the superclasses of class SELF.
Hides that Java uses a single inheritance."
  (declare #.*standard-optimize-settings*)
  (first (extends (extends-relation self))))

(defmethod direct-super-type ((self java-classifier-declaration))
  "RETURNS the first of the super-types of the java-classifier SELF."
  (declare #.*standard-optimize-settings*)
  (if (super-types self)
      (first (super-types self))
      '()))             

#.(declaim (inline java-classifier-equal))
(defun java-classifier-equal (classifier-1 classifier-2)
  "Compares two java-classifiers CLASSIFIER-1 and CLASSIFIER-2 
based on their type and signature."
  (declare #.*standard-optimize-settings*)
  (let ((first-classifier-type (type-of classifier-1)))
    (and 
     (subtypep first-classifier-type 'java-classifier-declaration)
     (eql first-classifier-type (type-of classifier-2))
     (equal (qualified-name classifier-1) (qualified-name classifier-2)))))

(defclass java-class-declaration (java-classifier-declaration)
  ()
  (:DOCUMENTATION "A Java class declaration"))

(defun java-class-declaration (parent-package source-file annotations modifiers signature
			       extends-relation implements-relation nested-classifiers fields methods)
  "Constructor"
  (declare #.*standard-optimize-settings*)  
  (java-classifier-declaration
   'java-class-declaration
   parent-package source-file annotations modifiers signature extends-relation implements-relation
   nested-classifiers fields methods))


(defclass java-enum-declaration (java-classifier-declaration) ;(java-classifier-declaration jana-enum-type)
  ()
  (:DOCUMENTATION "A Java enumeration declaration."))

(defun java-enum-declaration (parent-package source-file annotations modifiers signature
			      extends-relation implements-relation nested-classifiers fields methods)
  "Constructor"
  (declare #.*standard-optimize-settings*)
  (java-classifier-declaration
   'java-enum-declaration
   parent-package source-file annotations modifiers signature extends-relation implements-relation
   nested-classifiers fields methods))


(defclass java-interface-declaration (java-classifier-declaration)
  () 
  (:DOCUMENTATION "A Java interface declaration"))

(defun java-interface-declaration (parent-package source-file annotations modifiers signature
			           extends-relation implements-relation nested-classifiers fields methods)
  "Constructor"
  (declare #.*standard-optimize-settings*)
  (java-classifier-declaration
   'java-interface-declaration
   parent-package source-file annotations modifiers signature extends-relation implements-relation
   nested-classifiers fields methods))

(defclass java-annotation-declaration (java-interface-declaration)
  ()
  (:DOCUMENTATION "A Java 1.5 annotation type declaration"))

(defun java-annotation-declaration (parent-package source-file annotations modifiers signature
			           extends-relation implements-relation nested-classifiers fields methods)
  "Constructor"
  (declare #.*standard-optimize-settings*)
  (java-classifier-declaration
   'java-interface-declaration
   parent-package source-file annotations modifiers signature extends-relation implements-relation
   nested-classifiers fields methods))

;------------------------------------

(defclass java-variable (jana-variable)
  ((variable-declaration
    :DOCUMENTATION "The declaration of the variable."))
  (:DOCUMENTATION "Variables in the Java language are: static fields (class-variables),
fields (instance-variables), and local variables."))

(defmethod unqualified-name ((self java-variable))
  "Returns the qualfied-name of the variable, as defined in the variable declaration."
  (declare #.*standard-optimize-settings*)
  (unqualified-name (variable-declaration self)))

(defclass java-variable-declaration (jana-variable-declaration)
  ((jana-type
    :ACCESSOR jana-type
    :TYPE java-type)))

(defclass java-variable-reference (jana-variable-reference)
  ())

(defmethod unqualified-name ((self java-variable-declaration))
  "RETURNS the qualified name of the variable declaration."
  (declare #.*standard-optimize-settings*)
  (unqualified-name (signature self)))

(defclass java-local-variable-declaration (java-variable-declaration)
  ())

(defclass java-local-variable (java-variable)
  ((variable-declaration
    :accessor variable-declaration
    :initarg :variable-declaration
    :TYPE java-local-variable-declaration
    :DOCUMENTATION "A local variable declaration.")
   (variable-reference
    :DOCUMENTATION "The value used to reference a variable inside a method body."))
  (:DOCUMENTATION "In the Java language, local variables are declared and used inside methods."))

(defclass java-field-declaration (java-variable-declaration)
  ((field-annotations
    :ACCESSOR annotations
    :TYPE list
    :DOCUMENTATION "A list of annotations. Annotations are special modifiers that are declared
by a java annotation type.")
   (field-modifiers
    :ACCESSOR field-modifiers
    :TYPE java-modifiers
    :DOCUMENTATION "Modifiers for the field of a java classifier."))
   ;(initial-value
   ; :ACCESSOR initial-value
   ; :DOCUMENTATION "The initial value of the field.
   ;See: JVM Specification Chapter 2.5.1 'Initial Values of Variables'"))
  (:DOCUMENTATION "Java fields are further distinguished into instance variables,
 and class variables depending on the presence or absence of the modifier 'static"))

(defclass java-instance-variable-declaration (java-field-declaration)
  ()
  (:DOCUMENTATION "An instance variable is exclusively used by a single instance."))

(defclass java-instance-variable (java-variable)
  ((variable-declaration
    :accessor variable-declaration
    :initarg :variable-declaration
    :TYPE java-instance-variable-declaration
    :DOCUMENTATION "An instance-variable declaration.")
   (variable-reference
    :DOCUMENTATION "The value used to reference a field inside a method body."))
  (:DOCUMENTATION "A field of a class that is not declared static."))

(defclass java-class-variable-declaration (java-field-declaration)
  ()
  (:DOCUMENTATION "An class variable is shared among all instances of a class."))

(defclass java-class-variable (java-variable)
  ((variable-declaration
    :accessor variable-declaration
    :initarg :variable-declaration
    :TYPE java-class-variable-declaration
    :DOCUMENTATION "An instance-variable declaration.")
   (variable-reference
    :DOCUMENTATION "The value used to reference a static field inside a method body."))
  (:DOCUMENTATION "A field of a class that is declared static."))

(defun java-field-declaration (field-name field-annotations field-modifiers type)
    "Memoizing Constructor"
    (declare #.*standard-optimize-settings*
             (type string field-name)
	     (type list field-annotations)
	     (type java-modifiers field-modifiers)
	     (type java-type type))
    (mmake-java-field-declaration
     :constructor (make-java-field-declaration field-name field-annotations
					       field-modifiers type)
     :java-memoization-table *java-memoization-table*
     :keys (field-name type field-modifiers field-annotations)))

(defun make-java-field-declaration (field-name field-annotations field-modifiers type)
  "Protected Constructor"
  (declare #.*standard-optimize-settings*
           (type java-modifiers field-modifiers)
	   (type string field-name)) 
  (let ((instance))
    (setq instance
	  (make-instance
	   (if (member +STATIC-MODIFIER+ (modifier-list field-modifiers))
	       'java-class-variable-declaration
	       'java-instance-variable-declaration)
	   :signature (jana-name field-name)))
    (setf (annotations instance) field-annotations)
    (setf (field-modifiers instance) field-modifiers)
    (setf (jana-type instance) type)
    instance))

;------------------------------------

(defclass java-modifiers ()
    ((modifiers
      :initarg :modifiers
      :ACCESSOR modifier-list
      :TYPE list
      :DOCUMENTATION "A list of modifiers that apply to some Java language element,
like class, method, etc."))
  (:DOCUMENTATION "A Java modifier denotes properties, like visibility,
of fields, methods, classes, enums, or interfaces"))

(defun java-modifiers (modifiers-lispclass modifier-list)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type symbol modifiers-lispclass)
	   (type list modifier-list))
  (mmake-java-modifier
   :constructor (make-java-modifiers modifiers-lispclass modifier-list)
   :java-memoization-table *java-memoization-table*
   :keys (modifiers-lispclass modifier-list)))

(defun make-java-modifiers (modifiers-lispclass modifier-list)
  "Protected Constructor"
  (declare #.*standard-optimize-settings*
           (type symbol modifiers-lispclass)
	   (type list modifier-list))
  (when (debug-mode)
    (format t  "~%Modifiers: ~S" modifier-list)
    (format t  "~%Valid Modifiers: ~S" (valid-modifiers (make-instance modifiers-lispclass :modifiers modifier-list))))
  (let* ((instance
	  (make-instance modifiers-lispclass :modifiers modifier-list))
	 (valid-modifiers
	  (intersection (valid-modifiers instance) modifier-list)))
    ;(format t "~% ~A ~A" modifiers-lispclass valid-modifiers)
    (if (/= (length valid-modifiers) (length modifier-list))
	(error "Invalid ~S ~S ~S ~S"
	       modifiers-lispclass
	       valid-modifiers
	       modifier-list
	       (set-difference valid-modifiers modifier-list))
	instance)))

(defmethod valid-modifiers ((self java-modifiers))
  "RETURN: A list of valid modifiers."
  (declare #.*standard-optimize-settings*)  
   '(public private protected final static))


(defclass java-class-modifiers (java-modifiers)
  ()
  (:DOCUMENTATION "Class specific modifiers abstract, strictfp, enum, and annotation"))

(defun java-class-modifiers (modifier-list)
  "Constructor"
  (declare #.*standard-optimize-settings*)  
  (java-modifiers 'java-class-modifiers modifier-list))

(defmethod valid-modifiers ((self java-class-modifiers))
  "RETURN: A list of valid class modifiers."
  (declare #.*standard-optimize-settings*)
  (append (call-next-method)
	  '(abstract strictfp enum annotation synthetic)))

(defclass java-method-modifiers (java-modifiers)
  ()
  (:DOCUMENTATION "Method specific modifiers native, abstract, strictfp, and synchronized"))

(defun java-method-modifiers (modifier-list)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type list modifier-list))
  (java-modifiers 'java-method-modifiers modifier-list))

(defmethod valid-modifiers ((self java-method-modifiers))
  "RETURN: A list of valid method modifiers."
  (declare #.*standard-optimize-settings*)
  (append (call-next-method)
	  '(native abstract strictfp synchronized volatile synthetic bridge var-args)))

(defclass java-field-modifiers (java-modifiers)
  ()
  (:DOCUMENTATION "Field specific modifiers volatile, and transient"))

(defun java-field-modifiers (modifier-list)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type list modifier-list))
  (java-modifiers 'java-field-modifiers modifier-list))

(defmethod valid-modifiers ((self java-field-modifiers))
  "RETURN: A list of valid field modifiers."
  (declare #.*standard-optimize-settings*)
  (append (call-next-method)
	  '(volatile transient enum synthetic)))

(defclass java-annotation (jana-named-element)
  ((element-value-pairs
    :ACCESSOR element-value-pairs
    :INITFORM ()
    :TYPE list
    :DOCUMENTATION "A list of element value pairs."))
  (:DOCUMENTATION "A Java 1.5 annotation is a modifier consisting of the name of an annotation type
and zero or more element value pairs."))

(defun java-annotation (signature element-value-pairs)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type list element-value-pairs))
  (let ((instance))
    (setq instance
	  (make-instance 'java-annotation :signature signature))
    (setf (element-value-pairs instance) element-value-pairs)
    instance))

(defmethod qualified-name ((self java-annotation))
  (declare #.*standard-optimize-settings*)
  (qualified-name (signature self)))

;------------------------------------

(defclass java-implements (jana-generalization)
  ((super-types
    :ACCESSOR implements
    :INITFORM ()
    :INITARG :implements
    :DOCUMENTATION "The super-interface chains of implemented interfaces."))
  (:DOCUMENTATION "The Java implements relation."))

(defun java-implements (interfaces-list)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type list interfaces-list))
  (make-instance 'java-implements
		 :implements interfaces-list))

(defclass java-extends (jana-generalization)
  ((sub-types
    :INITFORM ()
    :INITARG :sub-types)
   (super-types
    :INITFORM ()
    :ACCESSOR extends
    :INITARG :extends))
  (:DOCUMENTATION "The Java inheritance relation."))

(defun java-extends (classes-list sub-classes)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type list classes-list sub-classes))
  (make-instance 'java-extends
		 :extends classes-list
		 :sub-types sub-classes))

(defmethod initialize-subclasses ((self java-classifier-declaration) project-context)
  "Initializes the sub-type relation of the java-project-context PROJECT-CONTEXT,
using the information provided by the java-classifier-declaration SELF."
  (declare #.*standard-optimize-settings*
           (type java-project-context project-context))
  (let ((relation (extends-relation self)))
    (when (and (slot-boundp relation 'super-types)
               (not (slot-boundp relation 'sub-types)))
      (add-direct-subtype project-context
                          (qualified-name (direct-super-type self))
                          (qualified-name self)))))

(defmethod initialize-implementors ((self java-classifier-declaration) project-context)
  "Initializes the implementor relation of the java-project-context PROJECT-CONTEXT,
using the information provided by the java-classifier-declaration SELF."  
  (declare #.*standard-optimize-settings*
           (type java-project-context project-context))
  (let ((relation (implements-relation self))
        (class-name (qualified-name self)))
    (when (slot-boundp relation 'super-types)
      (loop :for interface-super-type-chain :in (implements relation) 
            :do (add-direct-implementor project-context
                                        (qualified-name
                                         (first interface-super-type-chain))
                                        class-name)))))

;------------------------------------

(defclass java-method-declaration (jana-routine-declaration)
  ((argument-types
    :READER argument-types
    :INITARG :argument-types
    :TYPE list)
   (return-type
    :READER return-type
    :INITARG :return-type
    :TYPE java-type)
   (owner
    :ACCESSOR owner-class
    :INITARG :owner-class
    :TYPE java-signature
    :DOCUMENTATION "The owner class is the place where the method is declared.")
   (method-annotations
    :ACCESSOR annotations
    :TYPE list
    :DOCUMENTATION "A list of annotations. Annotations are special modifiers that are declared by
a java annotation type")
   (method-modifiers
    :READER method-modifiers
    :INITARG :method-modifiers
    :TYPE java-method-modifiers
    :DOCUMENTATION "Modifiers for the method")
   (thrown-exceptions
    :READER thrown-exceptions
    :INITARG :thrown-exceptions
    :TYPE list
    :DOCUMENTATION "A list of java-object-type elements that
correspond to the exceptions thrown by the method."))
  (:DOCUMENTATION "A method declaration."))

#.(declaim (inline java-method-equal))
(defun java-method-equal (method-1 method-2)
  "Compares two java-method-declarations METHOD-1 and METHOD-2 
based on their type and signature."
  (declare #.*standard-optimize-settings*)
  (let ((first-method-type (type-of method-1)))
    (and 
     (subtypep first-method-type  'java-method-declaration)
     (eql first-method-type (type-of method-2))
     (equal (qualified-name (owner-class method-1)) (qualified-name (owner-class method-2)))
     (equal (qualified-name method-1) (qualified-name method-2)))))

(defun java-method-declaration (annotations modifiers owner-class-qualified-name
				method-name return-type argument-types thrown-exceptions)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type string owner-class-qualified-name method-name)
	   (type java-method-modifiers modifiers)
	   (type java-type return-type)
	   (type list annotations argument-types thrown-exceptions))
  (when (verbose-mode)
    (format t "D"))
  (when (debug-mode)
    (format t "~% Method-Declaration: ~A(" method-name)
    (if argument-types
	(dolist-first-last (argument argument-types)
	  (format t "~A " (unqualified-name (signature argument)))
	  (format t "~A" (unqualified-name (signature argument)))))
	(format t ")"))
  (let ((instance))
    (setq instance
	  (make-instance 'java-method-declaration
			 :owner-class (java-signature owner-class-qualified-name)
			 :method-modifiers modifiers
			 :signature (jana-name method-name)
			 :return-type return-type
			 :argument-types argument-types
			 :thrown-exceptions thrown-exceptions))
    (setf (annotations instance) annotations)
    instance))

(defmethod qualified-name ((self java-method-declaration))
  (declare #.*standard-optimize-settings*)
  (qualified-name (signature self)))


(defclass java-closure (jana-closure)
  ()
  (:DOCUMENTATION "Abstract Class. This class exists only as a consequence of spearating the
language independent metamodel from the language dependent metamodel."))


(defclass java-method-implementation (jana-routine-implementation java-method-declaration)
  ((body
    :ACCESSOR body
    :TYPE java-closure))
  (:DOCUMENTATION "A method declaration + declaration of the method body.")) 

(defun java-method-implementation (annotations modifiers owner-class-qualified-name method-name
				   return-type argument-types thrown-exceptions body)
  "Constructor"
  (declare #.*standard-optimize-settings*)
   (when (verbose-mode)
    (format t "i"))
  (when (debug-mode)
    (format t "~% Method-Implementation: ~A(" method-name)
    (if argument-types
	(dolist-first-last (argument argument-types)
	  (format t "~A " (unqualified-name (signature argument)))
	  (format t "~A" (unqualified-name (signature argument)))))
	(format t ")"))
  (let ((instance))
    (setq instance (make-instance 'java-method-implementation
				  :owner-class (java-signature
						owner-class-qualified-name)
				  :method-modifiers modifiers
				  :signature (jana-name method-name)
				  :return-type return-type
				  :argument-types argument-types
				  :thrown-exceptions thrown-exceptions))
    (setf (annotations instance) annotations)
    (setf (body instance) body)
    instance))


