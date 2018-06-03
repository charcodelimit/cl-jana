;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Conversion of elements of the jana-metamodel for the Java-language
;;;   to the syntax of the Jimple intermediate representation.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 15 14:54:49 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:27 2010 (+0100)
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

(defpackage #:CL-JANA-JAVA-JIMPLE-CONVERSION
  (:USE :COMMON-LISP :JANA.BASE :JANA.METAMODEL :JANA.JAVA :JANA.JAVA.JIMPLE)
  (:NICKNAMES #:JANA.JAVA.JIMPLE.CONVERSION)
  (:IMPORT-FROM #:JANA.JAVA.JIMPLE)
  (:EXPORT #:jimple-statement
           #:write-jimple-statement
           #:write-debug-information
           #:save-java-project
           #:save-java-project-classes
           ; Java signatures
           #:write-java-method-signature
           #:write-full-java-classifier-signature
           #:write-full-java-method-signature
           #:write-full-java-field-signature)
  (:DOCUMENTATION
   "This package exports methods for converting elements of the Jimple
    Intermediate Representation of the Java programming language to
    the syntax of the Jimple Intermediate Representation.
    
    Christian Hofmann 2008 - 2009
    This package is provided under a BSD style license.
    See the source file for details."))