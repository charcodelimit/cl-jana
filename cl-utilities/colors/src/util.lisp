;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: :util.colors; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        util.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;  Miscellaneous utility functions and macros.
;;;  Contains the ONCE-ONLY macro from cl-utilities which is released under
;;;  the following license:
;;;    "The code in cl-utilities is in the public domain. Do whatever you want
;;;     with it."
;;;
;;;  "The ONCE-ONLY macro is hard to explain, hard to understand, hard to
;;;   write, hard to modify, and hard to live without once you figure out
;;;   how to use it. It's used in macros to guard against multiple
;;;   evaluation of arguments. My version is longer than most, but it
;;;   does some error checking and it gives gensym'd variables more
;;;   meaningful names than usual.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Sep 18 12:10:51 2009 (z)
;;; 
;;; Last-Updated: Fri Sep 18 12:15:33 2009 (z)
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

(in-package :util.colors)

(defun %check-once-only-names (names)
  "Check that all of the NAMES are symbols. If not, raise an error."
  ;; This only raises an error for the first non-symbol argument
  ;; found. While this won't report multiple errors, it is probably
  ;; more convenient to only report one.
  (let ((bad-name (find-if-not #'symbolp names)))
    (when bad-name
      (error "ONCE-ONLY expected a symbol but got ~S" bad-name))))

(defmacro once-only (names &body body)
  ;; Check the NAMES list for validity.
  (%check-once-only-names names)
  ;; Do not touch this code unless you really know what you're doing.
  (let ((gensyms (loop for name in names collect (gensym (string name)))))
    `(let (,@(loop for g in gensyms
                   for name in names
                   collect `(,g (gensym ,(string name)))))
       `(let (,,@(loop for g in gensyms for n in names
                       collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms
                         collect `(,n ,g)))
             ,@body)))))
