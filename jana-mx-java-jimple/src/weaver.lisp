;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        weaver.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 22 22:22:26 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:19 2010 (+0100)
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

(in-package :JANA.MX.JAVA.JIMPLE)

(defclass program-contexts ()
  ((class-level-scope
    :ACCESSOR class-level-scope
    :TYPE jana-local-scope
    :DOCUMENTATION "The class-level scope belonging to this classifier.")
   (instance-level-scope
    :ACCESSOR instance-level-scope
    :TYPE jana-local-scope    
    :DOCUMENTATION "The instance-level scope belonging to instances of this classifier.")
   (classifier-level-context
    :ACCESSOR class-level-context
    :TYPE jana-meta-level-context
    :DOCUMENTATION "The class-level context belonging to this classifier.")
   (instance-level-context
    :ACCESSOR instance-level-context
    :TYPE jana-meta-level-context
    :DOCUMENTATION "The instance-level context belonging to instances of this classifier.")
   (method-level-scope
    :ACCESSOR method-level-scope
    :TYPE jana-local-scope    
    :DOCUMENTATION "The scope associated with the closure."))
  (:DOCUMENTATION "The contexts and scopes of the base-program."))

;(defmethod transform ((self java-project) weaver)
;  (format t "~% ToDo"))

(defmethod add-context-variable ((self jimple-closure) (variable jimple-variable))
  "Add the java-context-variable VARIABLE to the jimple-closure SELF."
  (declare #.*standard-optimize-settings*)
  (let ((local-vars (local-variables self))
        (local-variable-declaration (variable-declaration variable)))
    (declare (type list local-vars)
             (type jimple-local-variable-declaration local-variable-declaration))
    (push local-variable-declaration
          local-vars)
    (setf (local-variables self)
          (nreverse local-vars))))

;; ------- INSERT-METHODS -------

(defmethod replace-instructions ((closure jimple-closure) instructions position &key (condition #'error))
  "Replaces the instructions in the jimple-closure CLOSURE starting
at position POSITION with the elements from the list INSTRUCTIONS.
If the list INSTRUCTIONS is longer than the list of instructions 
of the jimple-closure, a condition CONDITION is signaled."
  (let ((body (instructions closure)))
    (cond ((> (length instructions) (length body))
           (funcall condition "The provided instructions list is longer than the instructions list that shall be replaced!"))
          ((> (length instructions) 0)
           (replace (instructions closure)
                    instructions
                    :start1 position)))))

(defmethod update-branch-target-labels ((closure jimple-closure) instructions position)
  "Updates the branch-target labels such, that the first instruction in the list
INSTRUCTIONS gets the branch-target label assigned from the instruction at position
POSITION in the jimple-closure CLOSURE."
  (declare #.*standard-optimize-settings*
           (type fixnum position)
           (type list instructions))
  (let ((instruction-at-position (nth position (instructions closure)))
        (first-inserted-instruction (first instructions)))
    (when (and first-inserted-instruction
               instruction-at-position
               (slot-boundp instruction-at-position 'branch-target-label))
      (setf (branch-target-label first-inserted-instruction)
            (branch-target-label instruction-at-position))
      (setf (branch-target-label instruction-at-position) ""))))


(defmethod replace-first-instruction ((closure jimple-closure) instructions position)
  "Replaces the instruction at position POSITION in the jimple-closure CLOSURE,
with the first instruction from the list of instructions called INSTRUCTIONS.
All other instructions in the list INSTRUCTIONS are *inserted* after position POSITION
into the closure."
  (declare #.*standard-optimize-settings*
           (type fixnum position)
           (type list instructions))
  (update-branch-target-labels closure instructions position)
  (when (> (length instructions) 0)
    (setf (nth position (instructions closure))
          (car (instructions closure)))
    (when (> (length instructions) 1)
      (insert-instructions-before closure
                                  (cdr instructions)
                                  (+ position 1)))))

(defmethod private-add-instructions-before ((closure jimple-closure) instructions position)
  "Inserts the list of instructions INSTRUCTIONS before the element POSITION in the
list of instructions associated with the JIMPLE-CLOSURE closure.
This method does not update the branch-target labels!"
  (declare #.*standard-optimize-settings*
           (type fixnum position)
           (type list instructions))
  (if (= position 0)
      (setf (instructions closure)
            (nconc instructions (instructions closure)))
      (let ((split-instructions-list (nsplit-list (instructions closure) position))
            (modified-instructions-list '()))
        (declare (type cons split-instructions-list)
                 (type list modified-instructions-list))
        (setq modified-instructions-list
              (car split-instructions-list))
        (setq modified-instructions-list
              (nconc modified-instructions-list
                     instructions))
        (setq modified-instructions-list
              (nconc modified-instructions-list
                     (cdr split-instructions-list)))
        (setf (instructions closure)
              modified-instructions-list))))

;;; public interface
(defmethod insert-instructions-before ((closure jimple-closure) instructions position)
  "Inserts the list of instructions INSTRUCTIONS before the element POSITION in the
list of instructions associated with the JIMPLE-CLOSURE closure."
  (declare #.*standard-optimize-settings*
           (type fixnum position)
           (type list instructions))
  (update-branch-target-labels closure instructions position)
  (private-add-instructions-before closure instructions position))

(defmethod insert-instructions-after ((closure jimple-closure) instructions position)
  "Inserts the list of instructions INSTRUCTIONS after the element POSITION in the
list of instructions associated with the JIMPLE-CLOSURE closure.
If the position POSITION is -1, then the instructions are inserted before the head
of the instructions associated with the closure.
That is, the instructions associated with the closure are (destructively)
appended to the provided list of instructions."
  (declare (inline insert-instructions-before)
           (type fixnum position)
           (type list instructions))
  (if (< position (length (instructions closure)))
      (private-add-instructions-before closure instructions (+ position 1))
      (setf (instructions closure)
            (nconc (instructions closure)
                   instructions))))

;; not used ???
(defun add-static-call-first (class-name method-name return-type argument-types 
                                    bound-values method-implementation)
  "CLASS-NAME is the fully-qualified name of the callees class.
METHOD-NAME is the callee, METHOD-IMPLEMENTATION the caller.
The RETURN-TYPE, and ARG-TYPES are all attributes of the callee.
BOUND-VALUES can be obtained from the contexts associated with the caller."
  (let ((position (nth-value 1 (find-last-initialization-statement (body method-implementation))))
        (instructions '()))
    (format t "~%JoinPoint found at: ~A:~A" (qualified-name method-implementation) position)
    (setq instructions
          (list
           (make-static-invocation-instruction
            (jimple-reference-value-method
             method-name return-type argument-types)
            class-name
            bound-values)))
    (insert-instructions-after (body method-implementation)
                               instructions
                               position)))
                               