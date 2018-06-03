;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        packages.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;    Metamodel for the Jimple-Intermediate Representation of the Java Language.
;;;
;;;    This is a CLOS based implementation of a metamodel
;;;    for the Jimple IL. 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Aug 12 22:48:46 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:24:38 2010 (+0100)
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

(in-package :CL-USER)

(defpackage #:CL-JANA-JAVA-JIMPLE
  (:USE :COMMON-LISP :JANA.JAVA :JANA.METAMODEL :JANA.JAVA.BASE :JANA.BASE)
  (:NICKNAMES #:JANA.JAVA.JIMPLE)
  (:IMPORT-FROM #:JANA.JAVA)
  (:EXPORT ; metamodel
           #:jimple-closure
           #:jimple-variable
           #:jimple-instance-variable
           #:jimple-class-variable
           #:jimple-local-variable-declaration
           ; values
           #:jimple-reference-value           
           #:jimple-reference-value-local
           #:jimple-reference-value-field
           #:jimple-reference-value-class-variable
           #:jimple-reference-value-instance-variable
           #:jimple-reference-value-array
           #:jimple-reference-value-this
           #:jimple-reference-value-argument
           #:jimple-reference-value-caught-exception
           #:jimple-reference-value-method
           ; instructions
           #:jimple-instruction
           #:jimple-object-instantiation-instruction
           #:jimple-new
           #:jimple-new-array
           #:jimple-object-instruction
           #:jimple-cast           
           #:jimple-instanceof
           #:jimple-nop-instruction
           #:jimple-breakpoint-instruction
           #:jimple-length-instruction
           #:jimple-abstract-imaginary-assignment-instruction
           #:jimple-imaginary-instruction-assignment
           #:jimple-imaginary-instruction-variable-initialization
           #:jimple-conditional-critical-section-instruction
           #:jimple-ccsec-enter-instruction
           #:jimple-ccsec-exit-instruction
           ; control-transfer
           #:jimple-global-control-transfer-instruction
           #:jimple-method-invocation-instruction
           #:jimple-invoke-static-instruction
           #:jimple-dynamic-method-invocation-instruction
           #:jimple-invoke-interface-instruction
           #:jimple-invoke-special-instruction
           #:jimple-invoke-virtual-instruction
           #:jimple-throw-instruction
           #:jimple-imaginary-trap-instruction           
           #:jimple-local-control-transfer-instruction
           #:jimple-abstract-return-instruction
           #:jimple-return-instruction
           #:jimple-return-void-instruction
           #:jimple-branch-instruction
           #:jimple-goto-instruction
           #:jimple-if-instruction
           #:jimple-switch-instruction
           #:jimple-lookup-switch-instruction
           #:jimple-table-switch-instruction
           ; arithmetic
           #:jimple-arithmetic-instruction
           #:jimple-boolean-propositional-logic-instruction
           #:jimple-arithmetic-logic-instruction-and
           #:jimple-arithmetic-logic-instruction-or
           #:jimple-arithmetic-logic-instruction-xor
           #:jimple-arithmetic-logic-instruction-ushr
           #:jimple-arithmetic-instruction-add
           #:jimple-arithmetic-instruction-sub
           #:jimple-arithmetic-instruction-neg
           #:jimple-arithmetic-instruction-mul
           #:jimple-arithmetic-instruction-div
           #:jimple-arithmetic-instruction-rem
           #:jimple-arithmetic-instruction-shl
           #:jimple-arithmetic-instruction-shr
           #:jimple-arithmetic-comparison-instruction
           #:jimple-arithmetic-instruction-cmp
           #:jimple-arithmetic-instruction-cmpl
           #:jimple-arithmetic-instruction-cmpg
           #:jimple-arithmetic-predicate
           #:jimple-arithmetic-predicate-eq-p
           #:jimple-arithmetic-predicate-ne-p
           #:jimple-arithmetic-predicate-gt-p
           #:jimple-arithmetic-predicate-ge-p
           #:jimple-arithmetic-predicate-lt-p
           #:jimple-arithmetic-predicate-le-p
           ; constructors
           #:jimple-reference-value-from-declaration
           ; methods
           #:initialize-instructions
           #:initialize-branch-target-labels
           #:find-instruction-index
           #:reference-values-equal
           ; readers           
           #:thrown-exception
           #:java-type
           #:variable-name
           #:java-value
           #:arguments
           #:exception-types
           #:dimensions
           #:dimension-size-list
           #:local-variable
           #:default-target-label
           #:switch-argument-value
           #:field-name
           #:field-owner
           #:instructions
           #:branch-targets
           #:local-variables
           #:assignment-target
           #:assignment-source
           #:condition-statement
           #:index
           #:low-index
           #:high-index
           #:local-variable-instance-reference
           #:return-value
           #:receiver-class
           #:method-reference
           #:method-arguments
           #:method-name
           #:lookup-values
           #:start-label
           #:end-label
           #:handler-label
           #:handled-exception-type
           #:variable-declaration
           #:variable-reference
           #:instruction-index
           #:branch-target-label
           #:branch-target-table
           #:reference-value
           #:closure
           #:closure-context)
  (:DOCUMENTATION
   "This package exports classes for elements of the Jimple
    Intermediate Representation of the Java programming language.
    
    Christian Hofmann 2008 - 2009
    This package is provided under a BSD style license.
    See the source file for details."))