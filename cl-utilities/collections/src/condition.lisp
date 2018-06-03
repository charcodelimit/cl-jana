;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.collections; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        condition.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Oct  6 09:13:56 2009 (z)
;;; 
;;; Last-Updated: Tue Oct  6 09:18:03 2009 (z)
;;;           By: Christian Hofmann
;;; 
;;; Copyright 2009 Rudolph Neeser <rudy.neeser@gmail.com>.
;;; 
;;; This file is part of CL-HEAP
;;; 
;;; CL-HEAP is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; CL-HEAP is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with CL-HEAP.  If not, see <http://www.gnu.org/licenses/>.
;;; 
;;;****************************************************************************

;;;----------------------------------------------------------------

(in-package :util.collections)

;;;----------------------------------------------------------------

(define-condition heap-error (error)
  ((message :initform "An error has occured while using this data structure."
	    :initarg :message
	    :reader heap-error-message))
  (:documentation "The base condition type for all errors that should
  generally be thrown in the CL-HEAP package.")
  (:report (lambda (condition stream)
	     (format stream (heap-error-message condition)))))

(define-condition key-error (heap-error)
  ((message :initform "When using DECREASE-KEY, the HEAP-KEY function should always take two arguments.")))