;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        package.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; Created: Fri Sep 18 11:10:02 2009 (z)
;;; 
;;; Last-Updated: Fri Sep 18 15:51:18 2009 (z)
;;;           By: Christian Hofmann
;;; 
;;;****************************************************************************

(in-package :CL-USER)


(defpackage #:cl-utilities-colors
  (:use :COMMON-LISP)
  (:nicknames #:util.colors)
  (:export #:color
           #:rgb
           #:red
           #:green
           #:blue
	   #:rgba
           #:alpha
           #:add-alpha
	   #:hsv
           #:hue
           #:saturation
           #:value
	   #:rgb->hsv
           #:hsv->rgb
           #:->hsv
           #:->rgb
	   #:convex-combination
           #:hue-combination
           #:rgb-combination 
	   #:rgba-combination
           #:hsv-combination
           #:print-hex
           #:color-p))
