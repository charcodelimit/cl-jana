;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jun 12 15:41:56 2009 (z)
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


(in-package :CL-USER)

(defpackage #:CL-JANA-MX-JAVA-JIMPLE
  (:USE :COMMON-LISP :JANA.MX.JAVA :JANA.METAMODEL :JANA.JAVA :JANA.JAVA.JIMPLE :JANA.BASE :UTIL.COLLECTIONS :UTIL.GRAPH :UTIL.GRAPH.GXL)
  (:NICKNAMES #:JANA.MX.JAVA.JIMPLE)
  (:EXPORT #:java-instance-level-context
           #:java-method-context
           #:copy-jimple-closure
           ; scopes
           #:java-method-level-scope           
           ; contexts
           #:jimple-closure-context
           #:jimple-static-closure-context
           #:jimple-dynamic-closure-context
           #:jimple-instruction-level-context
           #:jimple-field-assignment-level-context
           #:jimple-field-write-context
           #:jimple-field-read-context
           ; methods
           #:this-reference-value
           #:write-field-access-statistics-to-file
           ; weaver
           #:weave-eraser-aspect
           #:add-synchronization-analysis)
  (:DOCUMENTATION
   "This package implements jimple specific middle-end functionality.
    
    Christian Hofmann 2009
    This package is provided under a BSD license.
    See the individual source files for details."))