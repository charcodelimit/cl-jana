;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-arithmetic-instructions.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Jimple Intermediate Representation of the Java Language.
;;;    Jimple Arithmetic Instructions.
;;;
;;;    This is a CLOS based implementation of the arithmetic instructions of
;;;    the Jimple Intermediate Representation of Java Sourcecode 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Aug 12 22:48:46 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:24:46 2010 (+0100)
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

(defmacro create-arithmetic-instruction-constructors (classname-list)
  "takes as argument a quoted list of classname symbols"
    (let ((l nil))
      (dolist (classname (eval classname-list))
	(push `(defun ,classname (argument-list)
		,(format nil "Constructor generated for class ~S" classname)
		(let ((instance (make-instance ',classname)))
		  (setf (arguments instance) argument-list)
		  instance))
	      l))
      (setq l (nreverse l))
      (append '(mapcar #'eval) (list (append '(quote) (list l))))))

;;; ---- class definitions

(defclass jimple-arithmetic-instruction (jimple-function jana-arithmetic-instruction)
  ((arguments
    :ACCESSOR arguments
    :TYPE list
    :DOCUMENTATION "One or more arguments to an arithmetic instruction."))
  (:DOCUMENTATION "Arithmetic instructions."))


(defclass jimple-boolean-propositional-logic-instruction (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION ""))

(defclass jimple-arithmetic-logic-instruction-and (jimple-boolean-propositional-logic-instruction)
  ()
  (:DOCUMENTATION "Boolean AND"))

(defclass jimple-arithmetic-logic-instruction-or (jimple-boolean-propositional-logic-instruction)
  ()
  (:DOCUMENTATION "Boolean OR"))

(defclass jimple-arithmetic-logic-instruction-xor (jimple-boolean-propositional-logic-instruction)
  ()
  (:DOCUMENTATION "Boolean XOR"))

(defclass jimple-arithmetic-logic-instruction-ushr (jimple-boolean-propositional-logic-instruction)
  ()
  (:DOCUMENTATION "Logical shift right"))

;; generate constructors
(create-arithmetic-instruction-constructors 
 '(jimple-arithmetic-logic-instruction-and
   jimple-arithmetic-logic-instruction-or
   jimple-arithmetic-logic-instruction-xor
   jimple-arithmetic-logic-instruction-ushr))

;;; ------
  
(defclass jimple-arithmetic-instruction-add (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Add"))

(defclass jimple-arithmetic-instruction-sub (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Subtraction"))

(defclass jimple-arithmetic-instruction-neg (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Negate"))

(defclass jimple-arithmetic-instruction-mul (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Multiply"))

(defclass jimple-arithmetic-instruction-div (jimple-arithmetic-instruction)
  ((potentially-thrown-exceptions
    :INITFORM `(,+java-arithmetic-exception+)))
  (:DOCUMENTATION "Divide"))

(defclass jimple-arithmetic-instruction-rem (jimple-arithmetic-instruction)
  ((potentially-thrown-exceptions
    :INITFORM `(,+java-arithmetic-exception+)))
  (:DOCUMENTATION "Remainder"))

(defclass jimple-arithmetic-instruction-shl (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Arithmetic shift left"))

(defclass jimple-arithmetic-instruction-shr (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Arithmetic shift right"))

;; generate constructors
(create-arithmetic-instruction-constructors
 '(jimple-arithmetic-instruction-add jimple-arithmetic-instruction-sub
   jimple-arithmetic-instruction-neg
   jimple-arithmetic-instruction-mul jimple-arithmetic-instruction-div
   jimple-arithmetic-instruction-rem
   jimple-arithmetic-instruction-shl jimple-arithmetic-instruction-shr))

;;; ------

(defclass jimple-arithmetic-comparison-instruction (jimple-arithmetic-instruction)
  ()
  (:DOCUMENTATION "Each floating-point type has two comparison instructions: fcmpl and fcmpg for type float,
and dcmpl and dcmpg for type double. 
The variants differ only in their treatment of NaN. NaN is unordered, so all floating-point comparisons fail if either of their operands is NaN. 
The compiler chooses the variant of the comparison instruction for the appropriate type that produces the same result whether the comparison 
fails on non-NaN values or encounters a NaN.\"
 
  \" 9 dload_1\"
  \"10 ldc2_w #4 // Push double constant 100.1\"
  \"13 dcmpg	  // To do the compare and branch we have to use...\"
  \"14 iflt 5	  // ...two instructions\"
  
  lcmp: \"Both value1 and value2 must be of type long. They are both popped from the operand stack, and a signed integer comparison is performed. 
         If value1 is greater than value2, the int value 1 is pushed onto the operand stack. 
         If value1 is equal to value2, the int value 0 is pushed onto the operand stack. 
         If value1 is less than value2, the int value -1 is pushed onto the operand stack.\"
  All citations from:  The Java(TM) Virtual Machine Specification"))

(defclass jimple-arithmetic-instruction-cmp (jimple-arithmetic-comparison-instruction)
  ()
  (:DOCUMENTATION "Compare"))


(defclass jimple-arithmetic-instruction-cmpl (jimple-arithmetic-comparison-instruction)
  ()
  (:DOCUMENTATION "Compare smaller"))

(defclass jimple-arithmetic-instruction-cmpg (jimple-arithmetic-comparison-instruction)
  ()
  (:DOCUMENTATION "Compare greater"))

;; generate constructors
(create-arithmetic-instruction-constructors
 '(jimple-arithmetic-instruction-cmp
   jimple-arithmetic-instruction-cmpl
   jimple-arithmetic-instruction-cmpg))

;;; ------

(defclass jimple-arithmetic-predicate (jimple-arithmetic-instruction jana-imaginary-instruction)
  ()
  (:DOCUMENTATION "Comparison between integer values"))

(defclass jimple-arithmetic-predicate-eq-p (jimple-arithmetic-predicate)
  ()
  (:DOCUMENTATION "=="))

(defclass jimple-arithmetic-predicate-ne-p (jimple-arithmetic-predicate)
  ()
  (:DOCUMENTATION "!="))

(defclass jimple-arithmetic-predicate-gt-p (jimple-arithmetic-predicate)
  ()
  (:DOCUMENTATION ">"))

(defclass jimple-arithmetic-predicate-ge-p (jimple-arithmetic-predicate)
  ()
  (:DOCUMENTATION ">="))

(defclass jimple-arithmetic-predicate-lt-p (jimple-arithmetic-predicate)
  ()
  (:DOCUMENTATION "<"))

(defclass jimple-arithmetic-predicate-le-p (jimple-arithmetic-predicate)
  ()
  (:DOCUMENTATION "<="))

;; generate constructors
(create-arithmetic-instruction-constructors
 '(jimple-arithmetic-predicate-eq-p jimple-arithmetic-predicate-ne-p
   jimple-arithmetic-predicate-gt-p jimple-arithmetic-predicate-ge-p
   jimple-arithmetic-predicate-lt-p jimple-arithmetic-predicate-le-p))





