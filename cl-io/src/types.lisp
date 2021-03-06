;;; -*- Mode: Lisp; Package: io.zip -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;;FILE:               types.lisp
;;;LANGUAGE:           Common-Lisp
;;;SYSTEM:             Common-Lisp
;;;USER-INTERFACE:     None
;;;DESCRIPTION
;;;    
;;;    Adds support for  GZip files to inflate.cl from Franz Inc.
;;;
;;;
;;;AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;;MODIFICATIONS
;;;    2009-05-28 <CHR> Initial Version.
;;;BUGS
;;;LEGAL
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


(in-package :io)

(deftype octet ()
  "Shorthand for \(UNSIGNED-BYTE 8)."
  '(unsigned-byte 8))
