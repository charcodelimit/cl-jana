;;; -*- Mode: Lisp; Package: jana.metamodel -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;;
;;; the package instructions contains the an abstraction for the actual languages.
;;; Instructions are abstracted into instruction types that are required
;;; or useable for symbolic execution.
;;;

(in-package #:JANA.METAMODEL)

(defclass jana-abstract-instruction ()
  ((line-number
    :ACCESSOR line-number
    :INITARG :line-number
    :TYPE fixnum
    :DOCUMENTATION "The line-number in the source-code corresponding to the instruction."))
  (:DOCUMENTATION "Statements executable on a virtual machine. The VM implements the operational semantics of the modeled programming language."))

(defclass jana-imaginary-instruction (jana-abstract-instruction)
  ()
  (:DOCUMENTATION "Virtual instructions are instructions added for analysis purposes.
 They provide primitives that ease analyzing more complex instructions
 in the languages to be analyzed.
 See: jana-virtual-assignment-instruction"))

(defclass jana-imaginary-assignment-instruction (jana-imaginary-instruction)
  ()
  (:DOCUMENTATION "Virtual assignment instructions are used to make variable initialization explicit. That is, an extra assignment instruction is added to the CFG instead of putting the values directly on the variable locations in the heap."))

(defclass jana-arithmetic-instruction (jana-abstract-instruction)
  ()
  (:DOCUMENTATION "Example: +, -, *"))

(defclass jana-control-transfer-instruction (jana-abstract-instruction)
  ()
  (:DOCUMENTATION "Transfer of Control."))


(defclass jana-global-control-transfer-instruction (jana-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Global transfer of control. Example: return"))

(defclass jana-local-control-transfer-instruction (jana-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Local transfer of control, within a control context
(might be limited to a compilation unit like a method in Smalltalk, or a .c file in C).
Example: if"))

(defclass jana-goto-instruction (jana-local-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Unconditional transfer of control."))

(defclass jana-branch-instruction (jana-local-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Conditional transfer of control. Example: if, while, switch"))


(defclass jana-routine-invocation-instruction (jana-global-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Example: function call"))
  
(defclass jana-return-instruction (jana-global-control-transfer-instruction)
  ()
  (:DOCUMENTATION "Explicitly models the return of control to the caller."))

(defclass jana-synchronization-instruction (jana-abstract-instruction)
  ()
  (:DOCUMENTATION "A synchronization instruction. Examples are: latches ,locks, barriers, etc."))

(defclass jana-conditional-critical-section-instruction (jana-synchronization-instruction)
  ()
  (:DOCUMENTATION "A conditional critical section. Examples are: monitors, semaphores, locks"))






 