;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.lispunit; Base: 10 -*- 
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
;;; Created: Mon Jul  6 16:09:06 2009 (z)
;;; 
;;; Last-Updated: Mon Jul  6 16:12:15 2009 (z)
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

(in-package :UTIL.LISPUNIT)

(defparameter *test-listener* nil)

(defparameter *tests* (make-hash-table))

;;; Used by RUN-TESTS to collect summary statistics
(defvar *test-count* 0)
(defvar *pass-count* 0)

;;; Set by RUN-TESTS for use by SHOW-FAILURE
(defvar *test-name* nil)

;;; If nil, errors in tests are caught and counted.
;;; If :ask, user is given option of entering debugger or not.
;;; If true and not :ask, debugger is entered.
(defparameter *use-debugger* nil)