;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        util.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Utility macros and functions
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sun Jun  7 22:33:02 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:23:48 2010 (+0100)
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
;;;****************************************************************************

(in-package :JANA.JAVA.BASE)

;; required by java-project
(defmacro assoc-value (value)
  "RETURNS cadr of the list VALUE if its cdr is a list, 
otherwise just the cdr is returned."
  `(if (listp (cdr ,value))
    (cadr ,value)
    (cdr ,value)))

;; required by java-global
(defmacro make-value-weak-synchronous-hashtable (&key (size 0) (test '#'eq))
  "Creates a weak hashtable that is accessed synchroneously in
CL-implementations that support multithreadings.
Support so far for: CCL, CLISP, LISPWORKS, SBCL
For CLISP and LISPWORKS weakness is provided, but no synchronization!"
  (append `(make-hash-table :size ,size :test ,test)
          #+CLISP '(:weak :value)
          #+SBCL '(:weakness :value :synchronized t)
          #+CCL '(:weak :value :shared :shared)
          #+LISPWORKS '(:weak-kind :value)))