;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-instructions.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Jimple Intermediate Representation of the Java Language.
;;;    Jimple Instructions.
;;;
;;;    This is a CLOS based implementation of the instructions of
;;;    the Jimple Intermediate Representation of Java Sourcecode 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Aug  8 23:19:00 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:24:43 2010 (+0100)
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

(defclass comet-instruction (jana-abstract-instruction)
  ((instruction-index
    :ACCESSOR instruction-index
    :TYPE fixnum
    :DOCUMENTATION "The original position in the instruction list.")
   (branch-target-label
    :ACCESSOR branch-target-label
    :TYPE string
    :INITFORM ""
    :DOCUMENTATION "The label used to identify the target of a local-control-transfer instruction.")
   (potentially-thrown-exceptions
    :ACCESSOR potentially-thrown-exceptions
    :TYPE list
    :INITFORM nil
    :DOCUMENTATION "The Java language specification identifies all instructions that may potentially throw an exception.
This slot sigifies the types of exceptions that may be thrown."))
  ;; chr: document better!
  (:DOCUMENTATION "MIXIN Class providing slots for information that is ascertained through analyses that are performed in addition to the JIMPLIFICATION pass of SOOT."))

(defclass jimple-instruction (comet-instruction)
  ()
  (:DOCUMENTATION "Instructions defined by the JIMPLE intermediate language."))

(defclass jimple-operation (jimple-instruction)
  ()
  (:DOCUMENTATION "Instructions that have only side-effects, and return no values."))

(defclass jimple-function (jimple-instruction)
  ()
  (:DOCUMENTATION "Instructions that return values."))

(defclass jimple-nop-instruction (jimple-operation)
  ()
  (:DOCUMENTATION "Do nothing.
This instruction is named after the Bytecode mnemonic 'nop'."))

(defun jimple-nop-instruction (line-number)
  "Constructor"
  (make-instance 'jimple-nop-instruction
                 :line-number line-number))

  
(defclass jimple-breakpoint-instruction (jimple-operation jana-imaginary-instruction)
  ()
  (:DOCUMENTATION "This instruction should represent the Java bytecode
'breakpoint' according to the Soot documentation. However,
as of The Java(TM) Virtual Machine Specification Second Edition, no
such mnemonic is defined.
Therefore this instruction is treated as an imaginary-instruction."))

(defun jimple-breakpoint-instruction (line-number)
  "Constructor"
  (make-instance 'jimple-breakpoint-instruction
                 :line-number line-number))


(defclass jimple-length-instruction (jimple-function)
  ((local-variable
    :READER local-variable
    :INITARG :local-variable
    :TYPE jimple-reference-value-local
    :DOCUMENTATION "a local variable, that has been assigned an array-reference."))
  (:DOCUMENTATION "calculates the length of an array that is assigned to a local variable.
This instruction roughly corresponds to the Java Bytecode mnemonic 'arraylength'.
However, the 2 address code requires that the array reference is loaded into a local variable,
whereas array-length takes as argument on the stack an object reference pointing to the array.."))

(defun jimple-length-instruction (local-variable)
  "Constructor"
  (make-instance 'jimple-length-instruction
		 :local-variable local-variable))

;;; -------

(defclass jimple-abstract-imaginary-assignment-instruction (jimple-operation jana-imaginary-assignment-instruction)
  ((assignment-target
    :ACCESSOR assignment-target
    :INITARG :assignment-target)
   (assignment-source
    :ACCESSOR assignment-source
    :INITARG :assignment-source))
  (:DOCUMENTATION "As Java Bytecodes are stack-based, no assignment to local variables takes place.
These are implicit in form of load-from and store-into kind of instructions, that allow
 to transfer data between the stack and constants, fields or references."))

(defclass jimple-imaginary-instruction-assignment (jimple-abstract-imaginary-assignment-instruction)
  ((assignment-target
    :DOCUMENTATION "A local variable or the reference to a field")
   (assignment-source
    :DOCUMENTATION "The source of the assignment can be a value: a local variable, field/array reference, or a constant.
Another source of an assignment instruction can be any instruction."))
   (:DOCUMENTATION "Assignment instructions are imaginary instructions, that assign to a targetVariable or targetField
a sourceValue, sourceVariable or the result of a sourceInstruction"))

(defun jimple-imaginary-instruction-assignment (target source line-number)
  "Constructor"
  (make-instance 'jimple-imaginary-instruction-assignment
		 :assignment-target target
		 :assignment-source source
                 :line-number line-number))

  
(defclass jimple-imaginary-instruction-variable-initialization (jimple-abstract-imaginary-assignment-instruction)
  ((assignment-target
    :TYPE jimple-reference-value-local
    :DOCUMENTATION "A local variable.")
   (assignment-source
    :TYPE jimple-reference-value
    :DOCUMENTATION "A reference to a field/array, this, or a argument."))
  (:DOCUMENTATION "This is a virtual instruction, as Jimple's local variable declaration and initialization statements 
have no direct Java Bytecode counterpart."))

(defun jimple-imaginary-instruction-variable-initialization (target source line-number)
  "Constructor"
  (declare (type jimple-reference-value-local target)
           (type jimple-reference-value source))
  (make-instance 'jimple-imaginary-instruction-variable-initialization
		 :assignment-target target
		 :assignment-source source
                 :line-number line-number))

;;; -------

(defclass jimple-object-instruction (jimple-function)
  ((java-type
    :READER java-type
    :INITARG :java-type
    :TYPE java-type
    :DOCUMENTATION "The type of the object."))
  (:DOCUMENTATION "Instructions specific to allocation of objects, instantiation of objects,
 type-conversion, and that like."))

;;;

(defclass jimple-instanceof (jimple-object-instruction)
  ((java-value
    :READER java-value
    :INITARG :java-value
    :TYPE java-value
    :DOCUMENTATION "A local variable or constant from which the type information is extracted."))
  (:DOCUMENTATION "An instruction that implements a predicate that compares all elements in the
supertype-chain of the value and retruns true if a match is found."))

(defun jimple-instanceof (value type)
  "Constructor"
  (make-instance 'jimple-instanceof
		 :java-value value
		 :java-type type))


(defclass jimple-cast (jimple-object-instruction)
  ((java-value
    :READER java-value
    :INITARG :java-value    
    :TYPE java-value
    :DOCUMENTATION "A local variable or constant that is to be accessed according to the protocol
defined by the type argument of the cast.")
   (potentially-thrown-exceptions
    :INITFORM `(,+java-class-cast-exception+)))
  (:DOCUMENTATION "An instruction that allows an instance to appear as one of its inherited types."))  
  
(defun jimple-cast (value type)
  "Constructor"
  (make-instance 'jimple-cast
		 :java-value value
		 :java-type type))

;;; instantiation

(defclass jimple-object-instantiation-instruction (jimple-object-instruction)
  ((potentially-thrown-exceptions
    :INITFORM `(,+java-out-of-memory-error+)))
  (:DOCUMENTATION "Instructions to instantiate objects."))

(defclass jimple-new (jimple-object-instantiation-instruction)
  ((java-type
    :TYPE java-reference-type))
  (:DOCUMENTATION "Instantiation of java reference types with the new statement.
Example: new Integer(12);"))

(defun jimple-new (reference-type)
  "Constructor"
  (make-instance 'jimple-new :java-type reference-type))


(defclass jimple-new-array (jimple-object-instantiation-instruction)
  ((array-dimensions
    :READER dimensions
    :INITARG :dimensions
    :TYPE fixnum)
   (dimension-size-list
    :READER dimension-size-list
    :INITARG :dimensions-size-list
    :TYPE list)
   (potentially-thrown-exceptions
    :INITFORM `(,+java-out-of-memory-error+ ,+java-negative-array-size-exception+)))
  (:DOCUMENTATION "Allocation of arrays.
Example new int[12];"))

(defun jimple-new-array (type array-dimensions sizes)
   "Constructor"
  (make-instance 'jimple-new-array
		 :java-type type
		 :dimensions array-dimensions
		 :dimensions-size-list sizes))

;;; ---- 

(defclass jimple-conditional-critical-section-instruction (jana-conditional-critical-section-instruction jimple-operation)
  ((java-value
    :READER java-value
    :INITARG :java-value
    :TYPE java-value
    :DOCUMENTATION "A local variable that contains a reference to the object whose lock is used.")
   (potentially-thrown-exceptions
    :INITFORM `(,+java-illegal-monitor-state-exception+)))
  (:DOCUMENTATION "The conditional critical section instructions correspond to the Java Bytecode instructions,
that claim or release the lock associated with each object."))

(defclass jimple-ccsec-enter-instruction (jimple-conditional-critical-section-instruction)
  ()
  (:DOCUMENTATION "Corresponds to the Java Bytecode mnemonic 'monitor-enter' that claims a lock."))

(defun jimple-ccsec-enter-instruction (java-value line-number)
  "Constructor"
  (make-instance 'jimple-ccsec-enter-instruction
		 :java-value java-value
                 :line-number line-number))


(defclass jimple-ccsec-exit-instruction (jimple-conditional-critical-section-instruction)
  ()
  (:DOCUMENTATION "Corresponds to the Java Bytecode mnemonic 'monitor-exit' that releases a lock."))

(defun jimple-ccsec-exit-instruction (java-value line-number)
  "Constructor"
  (make-instance 'jimple-ccsec-exit-instruction
		 :java-value java-value
                 :line-number line-number))