;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-metamodell.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Methods used to transform jimple-metamodel instances
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sun Jun 28 22:18:57 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:57 2010 (+0100)
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

(defmethod copy-jimple-closure ((method java-method-implementation))
  "Copy constructor creates a jimple closure that is compatible with the java-method implementation METHOD."
  (jimple-closure (return-type method)
		  (argument-types method)
                  (thrown-exceptions method)
		  nil
		  nil
		  nil))

(defmethod new-branch-target-label ((closure jimple-closure) &key (position -1))
  "RETURNS a new branch-target-label string based on the number of
labels in the jimple-closure CLOSURE."
  (let ((max-label-count
         (hash-table-count (branch-target-table closure)))
        (label ""))
    (setq label
          (format nil "$label~A" (+ max-label-count 1)))
    (setf (gethash label (branch-target-table closure))
          position)
    label))
          


;(defclass jimple-variable-reference (java-variable-reference)
;  ((closure
;    :READER closure
;    :INITARG :closure
;    :TYPE jimple-closure
;    :DOCUMENTATION "The closure in which the variable reference is used.")
;   (instruction-label
;    :READER instruction-label
;    :INITARG :instruction-label
;    :TYPE fixnum
;    :DOCUMENTATION "A label that is used to reference in the closure that instruction, where the variable is referenced.")
;   (reference-value
;    :READER reference-value
;    :INITARG :reference-value
;    :TYPE jimple-reference-value
;    :DOCUMENTATION "The value used to reference the variable."))
;  (:DOCUMENTATION "A variable reference. The attributes closure and instruction label uniquely determine a reference,
;because a variable may be referenced by multiple instructions."))

;(defun jimple-variable-reference (closure instruction-label reference-value)
;  "Constructor"
;  (declare #.*standard-optimize-settings*
;           (type jimple-closure closure)
;           (type fixnum instruction-label)
;           (type jimple-reference-value reference-value))
;  (make-instance 'jimple-variable-reference
;                 :closure closure
;                 :instruction-label instruction-label
;                 :reference-value reference-value))

