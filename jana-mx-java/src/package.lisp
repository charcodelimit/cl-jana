;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Java Specific MIddle-end eXtensions
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jun 12 15:07:08 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:30:14 2010 (+0100)
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

(defpackage #:CL-JANA-MX-JAVA
  (:USE :COMMON-LISP :JANA.MX :JANA.JAVA.JIMPLE.CONVERSION :JANA.JAVA.JIMPLE :JANA.JAVA :JANA.METAMODEL :JANA.BASE :JANA.JAVA.BASE)
  (:NICKNAMES #:JANA.MX.JAVA)
  (:IMPORT-FROM :JANA.MX)
  (:EXPORT #:load-java-metamodel
           ; contexts
           #:java-static-context
           #:java-dynamic-context
           #:java-classifier-context
           #:java-instance-context
           #:java-method-context
           ; scopes
           #:java-local-scope
           #:java-global-scope
           #:java-classifier-level-scope
           #:java-instance-level-scope
           #:context
           #:instance-level-scope
           #:class-level-scope
           #:instance-level-context
           #:class-level-context
           ; specials
           #:+this-context-variable-name+
           #:+this-class-context-variable-name+
           ; copy-constructors
           #:copy-java-type           
           #:copy-java-method-modifiers           
           #:copy-java-method-implementation
           ; methods
           #:gen-local-variable
           #:find-java-method
           #:direct-superclass
           #:find-java-method-in-class
           #:subclass-p
           #:this-reference-value
           ; readers
           #:global-scope
           #:project-context
           #:current-thread-context-variable
           #:this-context-variable
           #:this-class-context-variable
           #:dynamic-part
           #:static-part)
  (:DOCUMENTATION
   "This package exports Java specific middle-end functionality
    
    Christian Hofmann 2009
    This package is provided under a BSD license.
    See the individual source files for details."))