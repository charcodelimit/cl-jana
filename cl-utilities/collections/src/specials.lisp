;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        specials.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jul 17 22:42:51 2009 (z)
;;; 
;;; Last-Updated: Sun Oct  4 12:33:30 2009 (z)
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

(in-package :UTIL.COLLECTIONS)

;; -------- GLOBAL OPTIMIZATION SETTINGS ---------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *standard-optimize-settings*
; RECKLESS SPEED
;'(optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0))
; PRODUCTION
;'(optimize speed (safety 0) (space 0) (debug 1) (compilation-speed 0))
; DEVELOPMENT
;'(optimize speed (safety 3) (space 0) (debug 3) (compilation-speed 3))
    '(optimize
      speed
      (safety 3)
      (space 0)
      (debug 3)
      (compilation-speed 3))
    "The standard optimize settings used by most declaration expressions.")

  (defvar *fixnum-optimize-settings*
    '(optimize
      speed
      (safety 0)
      (space 0)
      (debug 1)
      (compilation-speed 0)
      #+:lispworks (hcl:fixnum-safety 0))
    "Like *STANDARD-OPTIMIZE-SETTINGS*, but \(on LispWorks) with all
arithmetic being fixnum arithmetic."))

(defmacro debug-mode () nil)

;; required by java-global
(defmacro make-synchronous-hashtable (&key (size 0) (test '#'eq))
  "Creates a hashtable that is accessed synchroneously in
CL-implementations that support multithreadings.
Support so far for: CCL, SBCL
For CLISP and LISPWORKS no synchronization is provided!"
  (append `(make-hash-table :size ,size :test ,test)
          #+SBCL '(:synchronized t)
          #+CCL '(:shared :shared)))