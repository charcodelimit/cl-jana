;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        specials.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Declarations of Constants
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Jun 23 13:36:26 2009 (z)
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

(in-package :JANA.MX.JAVA.JIMPLE)

;;; CONTEXTS

(defconstant +current-thread-context-variable-name+ "$mt"
    "The name of the local variable that is inserted into methods
in order to refer to the current thread.")

(defconstant +thread-class-signature+ "java.lang.Thread"
  "The signature of objects in the Java language that have type 'Thread'.")

;;; WEAVER

(defconstant +getclass-method-name+ "getClass"
  "The name of the method in class +CLASS-SIGNATURE+ that returns the
 class object for an instance.")

(defconstant +current-thread-method-name+ "currentThread"
  "The name of the method in class +THREAD-CLASS-SIGNATURE+ that returns the
 currently running thread instance.")

(defconstant +return-value-context-variable-prefix+ "$mcc_rval_"
  "The name prefix of the local variable that is inserted into methods
in order to refer to the return value of a method-call.")

(defconstant +argument-context-variable-prefix+ "$mcc_arg_"
  "The name prefix of the local variable that is inserted into methods
in order to refer to the argument of a method-call.")

(defconstant +exception-context-variable-prefix+ "$mcc_exc_"
  "The name prefix of the local variable that is inserted into methods
in order to refer to the exceptions thrown by a called method.")

(defconstant +this-field-prefix+ "this$"
  "The prefix for the field that stores the value of
the outer class instance used by Java compilers.")

(defconstant +this-field-prefix-length+ (length +this-field-prefix+)
  "The length of the prefix for the field that stores the value of
the outer class instance used by Java compilers.")