;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.base; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        contexts.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Provides classes to represent contexts in the jana metamodel
;;;   that allow to reference elements of the modeled programming language.
;;;   Contexts are used to transform the analyzed program such,
;;;   that this information becomes available at run-time.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jun 19 23:11:44 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:21:00 2010 (+0100)
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

(in-package :JANA.BASE)

(defclass jana-context ()
  ()
  (:DOCUMENTATION "Information about the program represented by the metamodel.
It should be noted that there are three levels here: the modeling or representation level,
the language meta-level, and the language level.
The modeling level is used by the compiler framework to represent the program.
The language meta-level is used to refer to elements of the program at compile- or run-time.
The language level allows the program to refer to elements like variables, types, or values at run-time."))

(defclass jana-compile-time-context (jana-context)
  ()
  (:DOCUMENTATION "Information that is available for introspection at compile-time."))

(defclass jana-run-time-context (jana-context)
  ()
  (:DOCUMENTATION "Information that is available for introspection by the program at run-time."))

(defclass jana-meta-level-context (jana-context)
  ()
  (:DOCUMENTATION "Information about the program itself."))

(defclass jana-language-level-context (jana-context)
  ()
  (:DOCUMENTATION "Information about the elements of the language, like variables, types, values, etc.
That is, things that the language allows to reference from within a program."))


