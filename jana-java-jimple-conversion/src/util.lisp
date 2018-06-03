;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.java.jimple.conversion; Base: 10 -*- 
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
;;; Created: Mon Jun 15 15:06:10 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:25:23 2010 (+0100)
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

(in-package :JANA.JAVA.JIMPLE.CONVERSION)

;; #.(declaim (inline write-quoted-name))
;; (defun write-quoted-name (name-string output-stream)
;;   "Quotes the string NAME-STRING with single-quotes '
;; when it is a  reserved java name and writes it to the stream
;; OUTPUT-STREAM."
;;   (declare #.*standard-optimize-settings*
;;            (type string name-string)
;;            (type stream output-stream))
;;   (cond ((find name-string +reserved-java-names+ :test #'equal)
;;          (write-char #\' output-stream)
;;          (write-sequence name-string output-stream)
;;          (write-char #\' output-stream))
;;         (t
;;          (write-sequence name-string output-stream))))
