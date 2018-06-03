;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.base; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        util.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;
;;;   Utility Functions 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Jun  3 11:24:50 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:20:51 2010 (+0100)
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


;; -------- DEBUG MACROS ---------

(defun toggle-debug-mode ()
  (cond
    ((member +FEATURE-DEBUG-MODE-SYMBOL+ *features*)
     (setq *features*
           (remove +FEATURE-DEBUG-MODE-SYMBOL+ *features*))
     (format t "~%Disabled  Jana Debug Output"))
    (t
     (push +FEATURE-DEBUG-MODE-SYMBOL+ *features*)
     (format t "~%Enabled  Jana Debug Output"))))

(defmacro debug-mode ()
  (if (member +FEATURE-DEBUG-MODE-SYMBOL+ *features*)
      't
      'nil))

;; -------- TIMING MACROS ----------

(defmacro print-time-elapsed (start end &key (message nil))
  "Generates a progn that prints a separator line, 
and the time elapsed between START end END  in seconds
together with a message that can be passed using the key :MESSAGE."
  `(progn
     (format t "~%~80{-~}" '(nil))
     ,(if message
          `(format  t ,(concatenate 'string "~%" message ": ~fs") (/ (- ,end ,start) cl:internal-time-units-per-second))
          `(format t "~%~fs" (/ (- ,end ,start) cl:internal-time-units-per-second)))))

;; -------- GLOBAL SETTINGS  ---------

(defun enable-multiprocessing ()
  (setq *multiprocessing* t)
  (format t "~%Enabled Multiprocessing in Jana"))

(defun disable-multiprocessing ()
  (setq *multiprocessing* nil)
  (format t "~%Disabled Multiprocessing in Jana"))

(defun toggle-verbose-mode ()
  (cond
    ((member +FEATURE-VERBOSE-MODE-SYMBOL+ *features*)
     (setq *features*
           (remove +FEATURE-VERBOSE-MODE-SYMBOL+ *features*))
       (format t "~%Disabled  Jana Verbose Output"))
    (t
      (push +FEATURE-VERBOSE-MODE-SYMBOL+ *features*)
      (format t "~%Enabled  Jana Verbose Output"))))

(defun verbose-mode ()
  (if (or
       (member +FEATURE-VERBOSE-MODE-SYMBOL+ *features*)
       (debug-mode))
      't
      'nil))

;; -------- UTILITY MACROS ---------

(defmacro dolist-first-last (dolist-args
			     dolist-body-first-element
			     dolist-body-last-element
			     &key ((:always dolist-body-always)))
  "First evaluates DOLIST-BODY-FIRST-ELEMENT for all elements in the
list, but the last. When the last element in the list is reached,
DOLIST-BODY-LAST-ELEMENT is evaluated."
  (let ((cnt (gensym))
	(end (gensym)))
  `(let ((,cnt 1)
	 (,end (length ,(second dolist-args))))
    ;(declare (type (integer ,cnt ,end)))
    (dolist ,dolist-args
      ,(when dolist-body-always dolist-body-always)
      (if (eq ,cnt ,end)
	  ,dolist-body-last-element
	  (progn (incf ,cnt)
		 ,dolist-body-first-element))))))

;; -------- LIST UTILITIES ---------

(defun split-list (list position)
  "SPLITS the list LIST non-destructively at the position POSITION,
 and RETURNS a cons containing the head and tail of the splitted list."
  (declare #.*fixnum-optimize-settings*
           (type list list)
           (type fixnum position))
  (when (> position (length list))
    (error "Can't split list of length ~A at position ~A." (length list) position))
  (cons (subseq list 0 position)
        (subseq list position)))

(defun nsplit-list (list position)
  "SPLITS the list LIST destructively at the position POSITION,
 and RETURNS a cons containing the head and tail of the splitted list,
where head is the modified LIST parameter."
  (declare #.*fixnum-optimize-settings*
           (type list list)
           (type fixnum position))
  (let ((tail (nthcdr position list)))
    (cond ((> position (length list))
           (error "Can't split list of length ~A at position ~A." (length list) position))
          ((> position 0)
           (rplacd (nthcdr (- position 1) list) nil)
           (cons list tail))
          (t 
           (cons '() tail)))))
