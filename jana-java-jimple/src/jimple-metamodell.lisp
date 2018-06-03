;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-metamodell.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Jimple Intermediate Representation of the Java Language.
;;;
;;;    This is a CLOS based implementation of a metamodel
;;;    for the Jimple Intermediate Representation of Java Sourcecode 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Thu Aug  7 22:57:41 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:24:42 2010 (+0100)
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

(defgeneric initialize-instructions (jimple-closure)
  (:DOCUMENTATION "Initializes the closure related informations of instructions."))

(defclass jimple-local-variable-declaration (java-local-variable-declaration)
  ((jana-type
    :INITARG :jana-type
    :TYPE java-type))
  (:DOCUMENTATION "Local variable declarations are typed in Jimple. Imaginary variables introduced
by the transformation into 3-address code are prefixed by #\$"))

(defun jimple-local-variable-declaration (name type)
  "Memoizing Constructor"
  (declare #.*standard-optimize-settings*
           (type string name)
	   (type java-type type))
  (mmake-java-local-variable-declaration
   :constructor (make-instance 'jimple-local-variable-declaration
			       :signature (jana-name name)
			       :jana-type type)
   :java-memoization-table *java-memoization-table*
   :keys (name type)))

(defclass jimple-variable (java-variable)
  ((variable-declaration
    :READER variable-declaration
    :INITARG :variable-declaration
    :TYPE jimple-local-variable-declaration
    :DOCUMENTATION "The declaration of the context variable.")
   (variable-reference
    :READER variable-reference
    :INITARG :variable-reference
    :TYPE jimple-reference-value
    :DOCUMENTATION "The reference used to refer to a variable."))
  (:DOCUMENTATION "Jimple context variables are local variables that are used to refer
to dynamic context, like the THIS reference, the current thread, etc."))

(defun jimple-variable (name type)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type string name)
	   (type java-type type))
  (make-instance 'jimple-variable
                 :variable-declaration (jimple-local-variable-declaration name type)
                 :variable-reference (jimple-reference-value-local name type)))

(defclass jimple-instance-variable (java-instance-variable)
  ((variable-reference
    :ACCESSOR variable-reference    
    :INITARG :variable-reference
    :TYPE jimple-reference-value-instance-variable        
    :DOCUMENTATION "The reference used to refer to a field."))
  (:DOCUMENTATION "Variables that are used to refer to instance variables (non-static fields)."))

(defun jimple-instance-variable (local-variable-instance-reference classifier-signature field-declaration)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type java-field-declaration field-declaration))
  (let ((reference-value
         (jimple-reference-value-instance-variable
          local-variable-instance-reference
          classifier-signature
          (jana-type field-declaration)
          (unqualified-name (signature field-declaration)))))
    (make-instance 'jimple-instance-variable
                   :variable-declaration field-declaration
                   :variable-reference reference-value)))

(defclass jimple-class-variable (java-class-variable)
  ((variable-reference
    :ACCESSOR variable-reference
    :INITARG :variable-reference
    :TYPE jimple-reference-value-class-variable            
    :DOCUMENTATION "The reference used to refer to a field."))
  (:DOCUMENTATION "Variables that are used to refer to class variables (static fields)."))

(defun jimple-class-variable (classifier-declaration field-declaration)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type java-classifier-declaration classifier-declaration)
           (type java-field-declaration field-declaration))
  (let ((reference-value
         (jimple-reference-value-class-variable
          (signature classifier-declaration)
          (jana-type field-declaration)
          (unqualified-name (signature field-declaration)))))
    (make-instance 'jimple-class-variable
                   :variable-declaration field-declaration
                   :variable-reference reference-value)))

(defclass jimple-closure (java-closure)
   ((instructions
     :ACCESSOR instructions
     :TYPE list
     :INITARG :instructions)
    (branch-target-table
     :READER branch-target-table
     :TYPE hash-table
     :INITARG :branch-target-table)
    (local-variables
     :ACCESSOR local-variables
     :TYPE list)
    (argument-types
     :READER argument-types
     :TYPE list
     :INITARG :argument-types)
    (exception-types
     :READER exception-types
     :TYPE list
     :INITARG :exception-types)
    (return-type
     :READER return-type
     :TYPE java-type
     :INITARG :return-type)
    (closure-context
     :ACCESSOR closure-context
     :DOCUMENTATION "The context accessible in a jimple-closure."))
   (:DOCUMENTATION "A closure whose instructions are valid statements of the Jimple Intermediate Language."))

(defun make-branch-target-table (association-list)
  "Creates and RETURNS a hash-table from the pairs stored in the association list ASSOCIATION-LIST.
Pairs must have the format (<string> . <fixnum>)!"
  (declare #.*standard-optimize-settings*
           (type list association-list))
  (let ((table (make-hash-table :test #'equal :size (length association-list))))
    (declare (type hash-table table))
    (loop :for pair :in association-list
          :do (setf (gethash (car pair) table)
                    (cdr pair))
          :finally (return table))))

(defun jimple-closure (return-type argument-types exception-types local-variables branch-target-label-map instructions)
  "Constructor"
  (let ((instance (make-instance 'jimple-closure
				 :instructions instructions
				 :branch-target-table (make-branch-target-table branch-target-label-map)
				 :argument-types argument-types
                                 :exception-types exception-types
				 :return-type return-type)))
    (setf (local-variables instance) local-variables)
    (initialize-instructions instance)
    instance))

(defmethod update-branch-target-table ((self jimple-closure))
  "Updates the branch-target-table of the jimple-closure SELF,
in order to reflect changes to the instructions list properly."
 (declare #.*standard-optimize-settings*)
 (let ((label "")
       (branch-target-table (branch-target-table self)))
  (loop
      :for instruction :in (instructions self)
      :for position :by 1
      :do
       (setq label (branch-target-label instruction))
       (unless (= (length label) 0)
         (unless (= position (gethash label branch-target-table))
           (setf (gethash label branch-target-table)
                 position))))))

(defmethod initialize-branch-target-labels  ((self jimple-closure))
  "Initializes new branch-target labels that were added to instructions
using the instructions position in the instructions list
of the jimple-closure SELF."
  (declare #.*standard-optimize-settings*)
  (let ((branch-target-label)
        (target-table (branch-target-table self)))
    (loop
        :for instruction :in (instructions self)
        :for position :of-type fixnum :by 1
        :do
         (setq branch-target-label
               (branch-target-label instruction))
         (when (and branch-target-label
                    (> (length branch-target-label) 0)
                    (< (gethash branch-target-label target-table) 0))
           (setf (gethash branch-target-label target-table)
                 position)))))

(defmethod initialize-instructions ((self jimple-closure))
  "Initializes the instruction labels for all instructions in a jimple-closure.
This method initializes the instruction-index according to the position in the instruction list,
and adds target labels using the information in the branch-target-table of the jimple-closure SELF."
  (declare #.*standard-optimize-settings*)
  ;; save original positions in instructions list
  (loop
      :for instruction :in (instructions self)
      :for position :of-type fixnum :by 1
      :do
       (setf (instruction-index instruction)
             position))
  ;; set branch-target labels
  (let ((instructions (instructions self)))
    (loop :for label :being :the hash-keys :in (branch-target-table self)
          :using (hash-value position)
          :do
           (if (< position 0) ;; only initialize valid labels
               (error "Found uninitialized label: ~A ~%Please use initialize-branch-target-labels before initializing the instructions."
                      label)
               (setf (branch-target-label (nth position instructions))
                     label)))))

(defmethod find-instruction-index ((self jimple-closure) instruction-index &key (starting-at 0))
  "RETURNs the position of the instruction with instruction-index INSTRUCTION-INDEX
in the instructions list of the jimple-closure SELF."
  (declare #.*standard-optimize-settings*
           (type fixnum instruction-index starting-at))
  (loop :for instruction :in  (nthcdr starting-at (instructions self))
        :for position :of-type fixnum :from starting-at :by 1
        :do (when (and (slot-boundp instruction 'instruction-index)
                       (= (instruction-index instruction) instruction-index))
              (return position))
        :finally (return -1)))