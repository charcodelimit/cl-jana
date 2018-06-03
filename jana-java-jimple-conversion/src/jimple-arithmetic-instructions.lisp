;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
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
;;; Last-Updated: Mi Jan  6 18:25:35 2010 (+0100)
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

(defmacro generate-write-jimple-statement-method (instruction-class
					    instruction-symbol-string
					    num-arguments)
  `(defmethod write-jimple-statement ((self ,instruction-class) stream)
    "RETURNS a string representation of the instruction that is used in the Jimple intermediate language."
    (declare #.*standard-optimize-settings*
             (type stream stream))
    ,(cond ((= num-arguments 1)
	    `(progn (write-sequence ,instruction-symbol-string stream)
	      (write-jimple-statement (first (arguments self)) stream)))
	   ((= num-arguments 2)
	      `(progn (write-jimple-statement (first (arguments self)) stream)
		(write-sequence ,instruction-symbol-string stream)
		(write-jimple-statement (second (arguments self)) stream))))))

;;; ------

(generate-write-jimple-statement-method
 jimple-arithmetic-logic-instruction-and " & " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-logic-instruction-or " | " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-logic-instruction-xor " ^ " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-logic-instruction-ushr " >>> " 2)

;;; ------

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-add " + " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-sub " - " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-neg "neg " 1)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-mul " * " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-div " / " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-rem " % " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-shl " << " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-shr " >> " 2)

;;; ------

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-cmp " cmp " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-cmpl " cmpl " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-instruction-cmpg " cmpg " 2)

;;; ------

(generate-write-jimple-statement-method
 jimple-arithmetic-predicate-eq-p " == " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-predicate-ne-p " != " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-predicate-gt-p " > " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-predicate-ge-p " >= " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-predicate-lt-p " < " 2)

(generate-write-jimple-statement-method
 jimple-arithmetic-predicate-le-p " <= " 2)
