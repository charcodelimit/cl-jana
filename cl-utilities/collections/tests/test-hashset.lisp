;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        test-hashset.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Sep 28 12:05:41 2009 (z)
;;; 
;;; Last-Updated: Fri Oct  2 23:53:44 2009 (z)
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

(in-package :util.collections)

(defun test-hash-set ()
  (set-test (make-hash-set) (make-hash-set))
  (set-test (make-bucket-hash-set) (make-bucket-hash-set)))
    

(defun set-test (h1 h2)
  (add-element 'a h1)
  (add-element 'b h1)
  (add-element 'c h1)
  ;; 
  (add-element 'b h2)
  (add-element 'c h2)
  (add-element 'd h2)
  ;;
  (format t "~%H1: ~A" (elements h1))
  (format t "~%H2: ~A" (elements h2))
  
  ;;(hashset-union h1 h2)
  
  (defvar *h3*)
  
  (setq *h3* (hashset-union h1 h2))
  
  (format t "~%H1 U H2: ~A" (elements *h3*)))