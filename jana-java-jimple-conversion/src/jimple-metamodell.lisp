;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
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
;;; Created: Mon Sep  1 22:34:27 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:30 2010 (+0100)
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

(in-package :JANA.JAVA.JIMPLE.CONVERSION)

;;; conversion of closures into Jimple statements

(defun make-type-variable-tree (local-variables)
  "Given a list of local-varibales, a tree is created that contains
types as nodes and lists of local-vriables as leafs.
The order in which types occur is fully preserved in the tree (L-R)
and the relative order of the occurence of variable names is preserved too."
  (declare #.*standard-optimize-settings*
           (type list local-variables))
  (let ((type-variable-map (make-hash-table))
	(type-list ())
	(current-type)
	(type-variablelist-pairs ())
        (local-variable-list ()))
    (declare (type hash-table type-variable-map)
	     (type list type-list type-variablelist-pairs local-variable-list))
    ;; append local variables to a list in a hash-table as they occur
    (dolist (local-variable local-variables)
      (setq current-type
            (jana-type local-variable))
      (setq local-variable-list 
            (gethash current-type type-variable-map))
      (unless local-variable-list
        (setq type-list
              (push current-type type-list)))
      (push local-variable local-variable-list)
      (setf (gethash current-type type-variable-map)
            local-variable-list))
    ;; traverse the list of types and create a tree node with the list of
    ;; local-variables as leaf
    (dolist (type type-list)
      (push 
       (list
        (with-output-to-string (s) (write-jimple-statement type s))
        (nreverse (gethash type type-variable-map)))
       type-variablelist-pairs))
    ;; return
    type-variablelist-pairs))

(defmethod jimple-local-variable-declaration-statement ((self jimple-closure) stream)
  "Writes the local variable declarations of a jimple-closure as jimple statements
to the output-stream STREAM.
This statement has the form:
\('        ' <type> \( <variable-name> ', ' \)* <variable-name> ';' '#\newline' \)*"
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (let ((type-variable-tree
	 (make-type-variable-tree (local-variables self))))
    (declare (type list type-variable-tree))
    (dolist (pair type-variable-tree)
      ;; convert type-node
      (write-sequence "        " stream)
      (write-sequence (first pair) stream)
      (write-char #\space stream)
      ;; convert local-variable list
      (dolist (local-variable (second pair))
	(if (eq local-variable
		(car (last (second pair))))
	    ; convert the last element in the list
	    (progn
	      (write-sequence (qualified-name (signature local-variable))
			      stream)
	      (write-char #\; stream)
	      (write-char #\newline stream))
	    (progn
	      (write-sequence (qualified-name (signature local-variable))
			      stream)
	      (write-char #\, stream)
	      (write-char #\space stream)))))
    ;; add a newline after the variable declarations
    (when type-variable-tree
      (write-char #\newline stream))))

(defmethod jimple-label-statement ((self jimple-closure) (instruction jimple-instruction) index stream)
  "Writes the Jimple-IL representation of the label that marks the instruction INSTRUCTION
,which is a branch target and is found at the index INDEX in the list of instructions of
the jimple-closure SELF, to the output-stream STREAM.
A label statement has the form:
'     ' 'label' <label-count> ':' '#\newline'"
  (declare #.*standard-optimize-settings*
           (type fixnum index)           
           (type stream stream))
  (let ((branch-target-label (branch-target-label instruction)))
    (declare (type string branch-target-label))
    (unless (= (length branch-target-label) 0)
      (unless (and (> index 0)
                   (typep (nth (- index 1) (instructions self)) 'jimple-branch-instruction))
        ;; add an additional newline before the label if the previous instruction
        ;; was no branch instruction (otherwise it is already there)
        (write-char #\newline stream))
      ;; look-up the label-statement
      (write-sequence "     " stream)
      (write-sequence (branch-target-label instruction) stream)
      (write-char #\: stream)
      (write-char #\newline stream))))

(declaim (inline jimple-instruction-statement))
(defmethod jimple-instruction-statement ((self jimple-closure) instruction index stream)
  "Writes the Jimple-IL representation of the jimple-instruction INSTRUCTION that is found
at the index INDEX in the list of instructions of the jimple-closure SELF
to the output-stream STREAM.
Instruction statements have the form:
 '        ' <instruction> ';' ('#\newline')?"
  (declare #.*standard-optimize-settings*
           (type jimple-instruction instruction)
           (type fixnum index)
           (type stream stream))
  (if (typep instruction 'jimple-local-control-transfer-instruction)
      (progn
        (when (and (> index 0)
                   (typep instruction 'jimple-imaginary-trap-instruction)
                   (not (typep (nth (- index 1) (instructions self)) 'jimple-imaginary-trap-instruction)))
          ;; add an additional newline before the first trap instruction
          (write-char #\newline stream))
        (write-sequence "        " stream)
        (jimple-local-control-transfer-statement instruction stream)
        (write-char #\; stream)
        (write-char #\newline stream)
        (when (typep instruction 'jimple-branch-instruction)
          ;; add an additional newline after a branch instruction
          (write-char #\newline stream)))
      ;; instructions that 'fall through'
      (progn
        (write-sequence "        " stream)
        (write-jimple-statement instruction stream)
        (write-char #\; stream)
        (write-char #\newline stream))))

(defmethod write-jimple-statement ((self jimple-closure) stream)
  "Writes the Jimple-IL representation of the jimple-closure SELF
to the output-stream STREAM.
See jimple-local-variable-declaration-statement,
jimple-label-statement, and jimple-instruction-statement for details on
the conversion."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (jimple-local-variable-declaration-statement self stream)
  (loop :for instruction :in (instructions self)
        :for index :from 0
        :do (progn (jimple-label-statement self instruction index stream)
                   (jimple-instruction-statement self instruction index stream))))

(defmethod write-jimple-statement ((self jimple-local-variable-declaration) stream)
  "Writes the Jimple-IL representation of a jimple-local-variable-declaration SELF
to the output-stream STREAM."
  (declare #.*standard-optimize-settings*
           (type stream stream))
  (write-sequence (qualified-name self) stream))