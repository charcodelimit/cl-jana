;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;
;;;   Package Definition
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Jun  3 11:29:41 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:29:27 2010 (+0100)
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

(in-package #:CL-USER)

(defpackage #:CL-JANA-METAMODEL
    (:nicknames #:JANA.METAMODEL)
  (:use :COMMON-LISP :JANA.BASE :JANA.JAVA.BASE)
  (:import-from :JANA.BASE)
  ;;; cat *.lisp | sed -n -e '/^(DEF/s/^(DEF/"/p'
  (:export ; metamodel elements
           #:jana-name
           #:jana-signature
           #:jana-named-element
           #:jana-module
           #:jana-abstract-module
           #:jana-imaginary-type
           #:jana-imaginary-module
           #:jana-type-declaration
           #:jana-reference-type-declaration           
           #:jana-routine-declaration
           #:jana-routine-implementation
           #:jana-variable
           #:jana-variable-declaration
           #:jana-variable-reference
           #:jana-generalization
           #:jana-closure
           ; typesystem
           #:jana-type
           #:jana-primitive-type
           #:jana-reference-type
           ; values
           #:jana-value
           #:jana-null-value
           #:jana-constant-value
           ; slot-names
           #:instructions           
           #:branch-targets
           #:local-variables
           #:argument-types
           #:return-type
           ; instructions
           #:jana-abstract-instruction
           #:jana-imaginary-instruction
           #:jana-imaginary-assignment-instruction
           #:jana-arithmetic-instruction
           #:jana-control-transfer-instruction
           #:jana-global-control-transfer-instruction           
           #:jana-routine-invocation-instruction           
           #:jana-local-control-transfer-instruction
           #:jana-branch-instruction
           #:jana-return-instruction
           #:jana-goto-instruction
           #:jana-synchronization-instruction
           #:jana-conditional-critical-section-instruction
           ; loader
           #:load-jana-metamodel
           ; methods
           #:signature
           #:qualified-name
           #:unqualified-name
           ; slots
           #:body
           ; readers
           #:name
           #:signature
           #:attributes
           #:routines           
           #:generalization-relation
           #:location
           #:variable-declaration
           #:line-number)
  (:documentation
   "This package exports classes for programming language elements.
    
    Christian Hofmann 2008 - 2009
    This package is provided under a BSD License.
    See the source file for details."))





