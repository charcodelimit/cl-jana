;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-utilities-collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        genhash-implementation-dependent.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Sep 16 10:17:04 2009 (z)
;;; 
;;; Last-Updated: Fri Sep 25 16:35:06 2009 (z)
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

(in-package :CL-UTILITIES-COLLECTIONS)

(defun make-generic-hash-table (&rest keys &key (size 17) (test #'eql) &allow-other-keys)
  "Constructor.
Contains some implementation specific hackery to whenever possible
make the generic hash-tables use the implementation specific hashtables
instead of the generic ones."
  (when (find :size keys)
    (remf keys :size))
  (when (find :test keys)
    (remf keys :test))
  (let ((hash-test (get-hash-test test))
        (hash-function (gensym))
        (test-function (gensym)))
    (unless hash-test
      (error 'unknown-hash :format-arguments (list test)))
    (if (builtin hash-test)
        ;; builtin test
        (apply #'make-hash-table (nconc `(:test ,test
                                          :size ,size)
                                        keys))
        ;; custom test
        (cond ((or (find :CCL cl:*features*)
                   (find :LISPWORKS cl:*features*))
               ;; hack to make CCL happy
               #+CCL (progn (setf (symbol-function hash-function)
                                  (hash-function hash-test))
                            (if (functionp test)
                                (setf (symbol-function test-function)
                                      test)
                                (setf test-function
                                      test))
                            (apply #'make-hash-table (nconc `(:test ,test-function
                                                              :size ,size
                                                              :hash-function ,hash-function)
                                                            keys)))
               #+LISPWORKS (apply #'make-hash-table (nconc `(:test ,test
                                                             :size ,size
                                                             :hash-function ,(hash-function hash-test)))))
              (t
               #-(OR CCL LISPWORKS)
               ;; else
               (let ((storage (make-array (list size) :initial-element nil)))
                 (warn "~A does not support the test-function ~A. Using generic hash-table implementation!"
                       (lisp-implementation-type) test)
                 (make-instance 'hash-container
                                :buckets storage
                                :stored-items 0
                                :allocated-buckets size
                                :test-designator (test-designator hash-test))))))))