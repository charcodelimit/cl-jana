;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Java Base Package
;;;
;;;  Defines the Interfaces and Classes responsible for Memoization
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Jun 16 00:09:09 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:54 2010 (+0100)
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

(in-package :CL-USER)

;private:
;#:project
;#:java-signature-cache

(defpackage #:CL-JANA-JAVA-BASE
  (:USE #:COMMON-LISP #:JANA.BASE)
  (:NICKNAMES #:JANA.JAVA.BASE)
  (:EXPORT ; specials
           #:*java-memoization-table*
           ; memoization
           #:java-memoization-table
           #:mmake-instance-using-env
           #:mmake-java-package
           #:mmake-java-classifier-declaration
           #:mmake-java-type
           #:mmake-java-modifier
           #:mmake-java-field-declaration
           #:mmake-java-value
           #:mmake-java-local-variable-declaration
           #:mmake-jimple-instruction
           #:mmake-java-signature
           #:mmake-jana-name
           ; contexts
           #:java-project-context
           ; scopes
           #:java-global-scope
           ; util
           #:assoc-value
           #:make-value-weak-synchronous-hashtable
           ; methods
           #:get-type
           #:add-direct-subtype
           #:add-direct-implementor
           ; readers
           #:java-types
           #:java-packages
           #:java-types-cache
           #:java-packages-cache
           #:java-classifier-declarations-cache
           #:project-context)
  (:DOCUMENTATION
   "This package exports classes and interfaces responsible
    for memoization.
    
    Christian Hofmann 2008 - 2009
    This package is provided under a BSD style license.
    See the source files for details."))
