;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.base; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        scopes.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Provides classes to represent scopes
;;;  of the programming language in the jana metamodel.
;;;  The main purpose of scopes is to provide variable instances
;;;  during transformations of the program meta-model.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jun 12 15:13:40 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:20:55 2010 (+0100)
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

(defclass jana-lexical-scope (jana-language-level-context jana-run-time-context)
  ((parent-scope
    :TYPE T
    :ACCESSOR parent-scope
    :DOCUMENTATION "The next more general scope."))
  (:DOCUMENTATION "A lexical scope. Lexical scopes are static contexts that provide variable instances
to transformations."))

(defclass jana-local-lexical-scope (jana-lexical-scope)
  ()
  (:DOCUMENTATION "A lexical scope of local extend."))

(defclass jana-global-lexical-scope (jana-lexical-scope)
  ((parent-scope
    :INITFORM NIL
    :DOCUMENTATION "The global scope is the top-level scope."))
  (:DOCUMENTATION "A lexical scope of global extend."))