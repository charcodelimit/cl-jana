;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-cflow-instructions.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Jimple Intermediate Representation of the Java Language.
;;;    Jimple Control Flow Instructions.
;;;
;;;    This is a CLOS based implementation of the control flow instructions of
;;;    the Jimple Intermediate Representation of Java Sourcecode  
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Aug 12 22:48:46 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:24:45 2010 (+0100)
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

(defclass comet-global-control-transfer-instruction (comet-instruction jana-global-control-transfer-instruction)
  ())

(defclass jimple-global-control-transfer-instruction (jimple-operation jana-global-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Instructions that cause a global transfer of control between methods."))

;;; ---- method-invocation ---

(defclass comet-method-invocation-instruction (comet-global-control-transfer-instruction)
  ((invocation-targets
    :ACCESSOR targets
    :DOCUMENTATION "Possible target methods of the method invocation caused by this instruction. 
This slot is not part "))
  (:DOCUMENTATION "MIXIN that extends method invocation instructions with possible call targets that are determined during
 intraprocedural control-flow analyses."))

(defclass jimple-method-invocation-instruction (jimple-function jimple-global-control-transfer-instruction comet-method-invocation-instruction)
  ((receiver-class
    :READER receiver-class
    :INITARG :receiver-class
    :TYPE java-reference-type
    :DOCUMENTATION "The type of the class that receives the method call.")
   (method-reference
    :READER method-reference
    :INITARG :method-reference
    :TYPE jimple-reference-value-method
    :DOCUMENTATION "a reference to the called method")
   (arguments
    :ACCESSOR method-arguments
    :TYPE list
    :INITARG :method-arguments
    :DOCUMENTATION "the arguments with which the method is called"))
  (:DOCUMENTATION "Instructions for method invocation."))

(defmethod argument-types ((self jimple-method-invocation-instruction))
  "RETURNS the types of the method-arguments using the information from the method-reference."
  (declare #.*standard-optimize-settings*)
  (argument-types (method-reference self)))

;;; static method-invocation

(defclass jimple-invoke-static-instruction (jimple-method-invocation-instruction)
  ()
  (:DOCUMENTATION "Named after the corresponding bytecode mnemnonic 'invokestatic'.
Invokes a class method.  If no implementation is found,
the call is dispatched up the supertype chain."))


(defun jimple-invoke-static-instruction (receiver-class method-reference arguments)
   "Constructor"
  (make-instance 'jimple-invoke-static-instruction
		 :receiver-class receiver-class
		 :method-reference method-reference
		 :method-arguments arguments))

;;; dynamic method-invocation
;;;   - interface
;;;        invocation of public methods defined by an interface, and super-type chain lookup starting with this
;;;   - special
;;;        i.e. invocation of private methods,
;;;              invocation of methods with lookup starting at the super-type,
;;;              constructor invocation
;;;   - virtual
;;;        public instance methods in the receiver object's class, or in a super-class of the receiver object

(defclass jimple-dynamic-method-invocation-instruction (jimple-method-invocation-instruction)
  ((local-variable
    :READER local-variable
    :TYPE jimple-reference-value-local
    :INITARG :local-variable
    :DOCUMENTATION "the receiving instance of the method call."))
  (:DOCUMENTATION "Instructions to invoke instance methods with the instance
referenced by the local-variable as receiver.

Dynamic method invocation
  - interface
     invocation of public methods defined by an interface,
     and super-type chain lookup starting with this
  - special
     i.e. invocation of private methods,
     invocation of methods with lookup starting at the super-type,
     constructor invocation
  - virtual
     public instance methods in the receiver object's class,
     or in a super-class of the receiver object"))

(declaim (inline jimple-dynamic-method-invocation-instruction))
(defun jimple-dynamic-method-invocation-instruction (invocation-instruction-type local-variable
						     receiver-class method-reference arguments)
  "Constructor"
  (declare (type jimple-reference-value-local local-variable)
	   (type java-reference-type receiver-class)
	   (type jimple-reference-value-method method-reference)
	   (type list arguments))
  (make-instance invocation-instruction-type
                 :local-variable local-variable
                 :receiver-class receiver-class
                 :method-reference method-reference
                 :method-arguments arguments))


(defclass jimple-invoke-interface-instruction (jimple-dynamic-method-invocation-instruction)
  ()
  (:DOCUMENTATION "Named after the corresponding bytecode mnemnonic 'invokeinterface'.
Invokes a  method using the instance that is referenced by the local variable.
The method has been declared in an interface that is implemented by the instance,
or inherited by the instance. This allows to use the protocol of the interface, instead
of using the protocol of the instance's type."))
  
(defun jimple-invoke-interface-instruction (local-variable receiver-class method-reference arguments)
  "Constructor"
  (jimple-dynamic-method-invocation-instruction 'jimple-invoke-interface-instruction
						local-variable receiver-class method-reference arguments))


(defclass jimple-invoke-special-instruction (jimple-dynamic-method-invocation-instruction)
  ()
  (:DOCUMENTATION "Named after the corresponding bytecode mnemnonic 'invokespecial'.
Invokes an instance method in a superclass of the current object,
or invokes private methods, and instance initialization methods of the current object,
which receives the method call."))

(defun jimple-invoke-special-instruction (local-variable receiver-class method-reference arguments)
   "Constructor"
  (jimple-dynamic-method-invocation-instruction 'jimple-invoke-special-instruction
						local-variable receiver-class method-reference arguments))


(defclass jimple-invoke-virtual-instruction (jimple-dynamic-method-invocation-instruction)
  ()
  (:DOCUMENTATION "Named after the corresponding bytecode mnemnonic 'invokevirtual'.
Invokes an instance method.  If no implementation is found,
the call is dispatched up the supertype chain according to the type of the instance."))

(defun jimple-invoke-virtual-instruction (local-variable receiver-class method-reference arguments)
   "Constructor"
  (jimple-dynamic-method-invocation-instruction 'jimple-invoke-virtual-instruction
						local-variable receiver-class method-reference arguments))


;;; --- returning of control from method invocations ---

(defclass jimple-abstract-return-instruction (jimple-global-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Abstract Class, Denotes all forms of returning control, with or without passing information."))

(defclass jimple-return-instruction (jimple-abstract-return-instruction jana-imaginary-instruction)
  ((return-value
    :READER return-value
    :INITARG :return-value
    :TYPE java-value
    :DOCUMENTATION "The value returned to the caller."))
  (:DOCUMENTATION "Control can be returned from a method call in Java with or without passing a value.
This becomes implicit in the Virtual machine through the passing of stack-frames.
This instruction is therefore an imaginary instruction that has only a correspondence in the Java language.
Example: return -12;"))

(defun jimple-return-instruction (return-value line-number)
  "Constructor"
  (make-instance 'jimple-return-instruction
		 :return-value return-value
                 :line-number line-number))


(defclass jimple-return-void-instruction (jimple-abstract-return-instruction)
  ()
  (:DOCUMENTATION "This instruction corresponds to the Java bytecode mnemonic 'return'.
It returns control to the caller when an invoked method reaches this instruction."))

(defun jimple-return-void-instruction (line-number)
  "Constructor"
  (make-instance 'jimple-return-void-instruction
                 :line-number line-number))

;;; --- non-local control transfer via exceptions ---

(defclass jimple-throw-instruction (jimple-global-control-transfer-instruction)
  ((thrown-exception
    :READER thrown-exception
    :INITARG :thrown-exception
    :TYPE java-value
    :DOCUMENTATION "The exception thrown by this throw-instruction.
The value can be an object-reference-value or null."))
  (:DOCUMENTATION "This instruction corresponds to the Java bytecode mnemonic 'athrow'.
The thrown exception is an object-type or the valuen null."))

(defun jimple-throw-instruction (value line-number)
  "Constructor"
  (make-instance 'jimple-throw-instruction
		 :thrown-exception value
                 :line-number line-number))

;;;;; -------------------------------------------

(defclass jimple-local-control-transfer-instruction (jimple-operation jana-local-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Instructions that cause a local transfer of control within methods."))

(defclass jimple-imaginary-trap-instruction (jimple-local-control-transfer-instruction jana-imaginary-instruction)
  ((start-label
    :READER start-label
    :INITARG :start-label
    :TYPE string
    :DOCUMENTATION "Instruction within a closure can be enumerated.
The start-label indicates the first instruction that can throw an exception that is handled by the trap.")
   (end-label
    :READER end-label
    :INITARG :end-label
    :TYPE string
    :DOCUMENTATION "Instruction within a closure can be enumerated.
The end-label indicates the last instruction that can throw an exception that is handled by the trap.")
   (handler-label
    :READER handler-label
    :INITARG :handler-instruction
    :TYPE string
    :DOCUMENTATION "Instruction within a closure can be enumerated.
Handler-label is a label that corresponds to the first instruction that handles the thrown exception.
This will be usually an assignment that assigns a jimple-reference-value-caught-exception to a local-variable.")
   (handled-exception-type
    :READER handled-exception-type
    :INITARG :handled-exception-type
    :TYPE java-object-reference-type
    :DOCUMENTATION "The exception passed to the handler."))
  (:DOCUMENTATION " Exception handling information is stored in the exception table of a class.
Therefore, Java Bytecode has no corresponding instruction.
The trap instruction is a separate part of the intermediate representation created from a method."))

(defun jimple-imaginary-trap-instruction (start-label end-label handler-label exception-type)
  "Constructor"
  (make-instance 'jimple-imaginary-trap-instruction
		 :start-label start-label
		 :end-label end-label
		 :handler-instruction handler-label
		 :handled-exception-type exception-type))


(defclass jimple-branch-instruction (jimple-local-control-transfer-instruction jana-branch-instruction)
  ((branch-target-labels
    :READER branch-targets
    :INITARG :branch-targets
    :TYPE list
    :DOCUMENTATION "Instruction within a closure can be enumerated.
Branch-target-labels is a list of indices that correspond to a jimple-instruction within a jimple-closure."))
  (:DOCUMENTATION "Jimple Instructions where the control flow branches."))


(defclass jimple-goto-instruction (jimple-branch-instruction)
  ()
  (:DOCUMENTATION "Named after the corresponding bytecode mnemnonic 'goto'.
This instruction causes the control flow to branch to the instruction in the current
closure that is denoted by the index."))

(defun jimple-goto-instruction (target-label line-number)
  "Constructor"
  (make-instance 'jimple-goto-instruction
		 :branch-targets (list target-label)
                 :line-number line-number))
	       

(defclass jimple-if-instruction (jimple-branch-instruction jana-imaginary-instruction)
  ((condition-statement
    :READER condition-statement
    :INITARG :condition-statement
    :TYPE jimple-arithmetic-predicate
    :DOCUMENTATION "A predicate instruction that returns a boolean value on which the executed decided is decided."))
  (:DOCUMENTATION "This instruction corresponds to the 'if_*' opcodes of Java VM Bytecode. It is an imaginary instruction,
because different if-branch instructions are distinguished in Java Bytecode depending on the predicate in the
condition-statement.
The if-instructions branches, when the predicate instruction evaluates to true, otherwise execution continues with the next
 instruction."))

(defun jimple-if-instruction (condition-statement target-label line-number)
  "Constructor"
  (make-instance 'jimple-if-instruction
		 :branch-targets (list target-label)
		 :condition-statement condition-statement
                 :line-number line-number))


(defclass jimple-switch-instruction (jimple-branch-instruction)
  ((default-target-label
     :READER default-target-label
     :INITARG :default-target-label
     :TYPE string
     :DOCUMENTATION "Instruction within a closure can be enumerated.
The default-target-label designates the instruction that is executed when the switch-argument's value does not match any of
the values defined in the switch-statement.")
   (switch-argument-value
    :READER switch-argument-value
    :INITARG :switch-argument-value
    :TYPE java-value
    :DOCUMENTATION "Based on this value the branch-target is selected."))
  (:DOCUMENTATION "Branch instructions that decide the branch-target based on a mapping from values to branch-targets."))       

(defclass jimple-lookup-switch-instruction (jimple-switch-instruction)
  ((lookupValues
    :READER lookup-values
    :INITARG :lookup-values
    :TYPE list
    :DOCUMENTATION "A list of constant values that are the keys corresponding to the target-label
that is stored in the same position of the list in the default-target-label slot."))
  (:DOCUMENTATION "This instruction corresponds to the Java Bytecode mnemonic 'lookupswitch'.
A list of keys and target instrucions that is used to determine the branch-targets depending on the value of the
switch statement's argument."))

(defun jimple-lookup-switch-instruction (local-variable lookup-values target-labels default-target-label line-number)
  "Constructor"
  (make-instance 'jimple-lookup-switch-instruction
		 :switch-argument-value local-variable
		 :lookup-values lookup-values
		 :branch-targets target-labels
		 :default-target-label default-target-label
                 :line-number line-number))


(defclass jimple-table-switch-instruction (jimple-switch-instruction)
  ((low-index
    :READER low-index
    :INITARG :low-index
    :TYPE fixnum
    :DOCUMENTATION "The minimum value compared with the argument.")
   (high-index
    :READER high-index
    :INITARG :high-index
    :TYPE fixnum
    :DOCUMENTATION "The maximum value compared with the argument."))
  (:DOCUMENTATION "This instruction corresponds to the Java Bytecode mnemonic 'tableswitch'.
A table with size (high-index - low-index) is used to store the keys that are compared to the argument
and the branch-targets. This is representation is chosen by the compiler, when the key-values are all
numerically preceding each other."))

(defun jimple-table-switch-instruction (local-variable low-index high-index target-labels default-target-label line-number)
  "Constructor"
(make-instance 'jimple-table-switch-instruction
		 :switch-argument-value local-variable
		 :low-index low-index
		 :high-index high-index
		 :branch-targets target-labels
		 :default-target-label default-target-label
                 :line-number line-number))