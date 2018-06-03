;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl.zip; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;; FILE: specials.lisp
;;;  LANGUAGE: Common-Lisp
;;;
;;;  DESCRIPTION
;;;
;;;    Special Variable Declarations
;;; 
;;; AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;; MODIFICATIONS
;;;    2009-05-29 <CHR> Initial Version.
;;; BUGS
;;;
;;; LEGAL
;;;    
;;;    Copyright C. Hofmann 2009
;;;    mailto:c.hofmann@utwente.nl
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by 
;;; the Free Software Foundation, as clarified by the AllegroServe
;;; prequel found in license-allegroserve.txt.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file 
;;; license-lgpl.txt that was distributed with this file.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;;; Suite 330, Boston, MA  02111-1307  USA
;;;    
;;;***************************************************************************

(in-package :io.zip)

(defvar *standard-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by most declaration expressions.")

(defvar *fixnum-optimize-settings*
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "Like *STANDARD-OPTIMIZE-SETTINGS*, but \(on LispWorks) with all
arithmetic being fixnum arithmetic.")

(defstruct gzip-constants
    "A structure defining constants header-fields
that are used to identify valid gzip-files."
    (magic (make-array 2 :element-type 'octet :initial-contents #(#x1f #x8b)) :type (vector (octet) *))
    (deflate-compression #x8 :type octet))

(defmethod make-load-form ((structure gzip-constants) &optional environment)
  (make-load-form-saving-slots structure :environment environment))

(defconstant +GZIP-CONSTANTS+ (make-gzip-constants)
  "Constant header fields.")

;; -------- DEBUG MACROS ---------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +FEATURE-DEBUG-MODE-SYMBOL+ :IO-ZIP-DEBUG)
  (defconstant +FEATURE-VERBOSE-MODE-SYMBOL+ :IO-ZIP-VERBOSE))