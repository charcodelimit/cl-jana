;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  The top-level package for Jana that defines
;;;  the public interface.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jun 15 18:40:18 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:18:09 2010 (+0100)
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

(in-package :cl-user)

(defpackage #:CL-JANA
    (:nicknames #:JANA)
  (:use :COMMON-LISP :JANA.BASE :JANA.JAVA :JANA.JAVA.JIMPLE.CONVERSION :JANA.MX.JAVA :JANA.MX.JAVA.JIMPLE)
  (:export #:load-java-project
           #:main
           #:load-java-classes
           #:save-java-project
           #:add-synchronization-analysis
           #:identity-transformation
           #:write-field-access-statistics-to-file
           #:toggle-debug-mode
           #:toggle-verbose-mode
           #:enable-multiprocessing
           #:disable-multiprocessing)
  (:documentation
   "The external interface to Jana.
    
    Christian Hofmann 2008 - 2009
    This package is provided under a BSD License.
    See the source file for details."))
