;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jul 17 22:35:16 2009 (z)
;;; 
;;; Last-Updated: Fr Nov 20 13:43:11 2009 (+0100)
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

(in-package :CL-USER)

(defpackage #:cl-utilities-collections
  (:use :COMMON-LISP)
  (:nicknames #:util.collections)
  (:export #:hash-set
           #:fast-hash-set
           #:key-hash-set
           #:bucket-hash-set
           #:generic-hash-set
           #:make-hash-set
           #:make-fast-hash-set
           #:make-key-hash-set
           #:make-bucket-hash-set
           #:make-generic-hash-set
           #:hashset-table
           #:copy-set
           #:add-element
           #:replace-element
           #:remove-element
           #:remove-bucket
           #:collect-values
           #:elements
           #:hashset-count
           #:hashset-union
           #:hashset-nunion           
           #:contains-value-p
           #:map-elements           
           #:doset
           #:do-bucketset
           #:copy-hash-table
           ;;; heaps
           #:heap
	   #:binary-heap
	   #:fibonacci-heap
	   #:heap-key
	   #:heap-sorting-function
	   #:add-to-heap
	   #:add-all-to-heap
	   #:peep-at-heap
	   #:pop-heap
	   #:heap-size
	   #:empty-heap
	   #:is-empty-heap-p
	   #:merge-heaps
	   #:nmerge-heaps
	   #:decrease-key
	   #:delete-from-heap
	   #:binary-heap-extension-factor
	   #:priority-queue
	   #:empty-queue
	   #:queue-size
	   #:peep-at-queue
	   #:dequeue
	   #:enqueue
	   #:heap-error
	   #:key-error
	   #:fibonacci-test
	   #:binary-test
	   #:priority-queue-test)
  (:documentation
   "This package exports Common-Lisp utility functionality for collections.
    
    Christian Hofmann 2009
    This package is provided under a modified BSD License.
    See the individual source files for details."))