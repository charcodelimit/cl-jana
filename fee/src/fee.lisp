;;; -*- Mode: Lisp; Package: java.jana.metamodel -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;;FILE:               fee.lisp
;;;LANGUAGE:           Common-Lisp
;;;SYSTEM:             Common-Lisp
;;;USER-INTERFACE:     None
;;;DESCRIPTION
;;;    
;;;    Metamodel for the Java Language.
;;;
;;;    This is a CLOS based implementation of a metamodel
;;;    for Java 
;;;
;;;
;;;
;;;AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;;MODIFICATIONS
;;;    2008-12-04 <CHR> Initial Version.
;;;BUGS
;;;LEGAL
;;;    GPL
;;;    
;;;    Copyright C. Hofmann 2008 - 2008
;;;    mailto:c.hofmann@utwente.nl
;;;    
;;;    This program is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU General Public License
;;;    as published by the Free Software Foundation; either version
;;;    2 of the License, or (at your option) any later version.
;;;    
;;;    This program is distributed in the hope that it will be
;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;    PURPOSE.  See the GNU General Public License for more details.
;;;    
;;;    You should have received a COPY of the GNU General Public
;;;    License along with this program; if not, write to the Free
;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;    Boston, MA 02111-1307 USA
;;;***************************************************************************

(in-package :CL-USER)

(defpackage #:FEE
  (:USE  :COMMON-LISP)
  (:EXPORT #:fee-server
           #:make-fee-server
           #:connect-to-fee-server
           #:set-compact-mode
           #:set-project-jar-file
           #:analyze
           #:analyze-project
           #:analyze-aspects-in-project)
  (:DOCUMENTATION
   "This package exports classes for programming language elements.
    
    Christian Hofmann 2008 - 2008
    This package is provided under the GNU General Public License.
    See the source file for details."))