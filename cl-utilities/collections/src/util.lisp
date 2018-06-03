;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        util.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Sep 28 20:46:06 2009 (z)
;;; 
;;; Last-Updated: Mon Sep 28 20:46:52 2009 (z)
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

(defun copy-hash-table (table &key test size rehash-size rehash-threshold)
  "RETURNS a shallow copy of the hash-table TABLE."
  (setf test
        (or test (hash-table-test table)))
  (setf size
        (or size (hash-table-size table)))
  (setf rehash-size
        (or rehash-size (hash-table-size table)))
  (setf rehash-threshold
        (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((new-table (make-hash-table :test test
                                    :size size
                                    :rehash-size rehash-size
                                    :rehash-threshold rehash-threshold)))                     
    (maphash (lambda (key value)
               (setf (gethash key new-table) value))
             table)
    new-table))
