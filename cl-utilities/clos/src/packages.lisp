;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-user; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        packages.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jul 17 22:36:53 2009 (z)
;;; 
;;; Last-Updated: Fri Jul 17 22:37:04 2009 (z)
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
;;; -*- Mode: Lisp; Package: cl-user -*-
;;; -*- coding:utf-8 -*-
;;;***************************************************************************
;;;FILE:               cl-utilities.lisp
;;;LANGUAGE:           Common-Lisp
;;;SYSTEM:             Common-Lisp
;;;USER-INTERFACE:     None
;;;DESCRIPTION
;;;    
;;;    Utility functions for Common-Lisp.
;;;
;;;    Contains CLOS copy constructor functionality.
;;;
;;;
;;;
;;;AUTHORS
;;;    <CHR> Christian Hofmann <c.hofmann@utwente.nl>
;;;MODIFICATIONS
;;;    2008-12-28 <CHR> Initial Version.
;;;BUGS
;;;LEGAL
;;;    
;;;    Copyright C. Hofmann 2008 - 2009
;;;    mailto:c.hofmann@utwente.nl
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the \"Software\"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the \"Software\"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;    
;;;***************************************************************************

(in-package :CL-USER)

(defpackage #:CL-UTILITIES-CLOS
  (:USE #:COMMON-LISP)
  (:NICKNAMES #:UTIL.CLOS)
  (:DOCUMENTATION
   "This package exports Common-Lisp utility functionality.
    
    Christian Hofmann 2008 - 2009
    This package is provided under the GNU General Public License.
    See the individual source files for details."))

(in-package :util.clos)

;; from mop-feature-tests (c) Pascal Costanza, BSD License
(defparameter *mop-package-name*
  #+allegro "MOP" ;; "CLOS" "ACLMOP"
  #+clisp "CLOS"
  #+cmu "CLOS-MOP" ;; "MOP"
  #+ecl "CLOS"
  #+gcl "PCL"
  #+lispworks "CLOS"
  #+(or mcl openmcl) "CCL"
  #+sbcl "SB-MOP")

(defun make-temp-package (name symbol-list) 
  "Creates a temporary package with name NAME that imports all the
symbols from the mop package of the used COMMON-LISP implementation.
This allows to uniformly refer to the mop-package with NAME:symbol."
  (unless (find-package name)
    (eval `(defpackage ,name
	    (:use "COMMON-LISP" ,*mop-package-name*)
	    (:shadowing-import-from ,*mop-package-name*
	     ,@(loop for sym being the external-symbols of :common-lisp
		     when (member (symbol-name sym)
				  (package-shadowing-symbols *mop-package-name*)
				  :test #'string=
				  :key #'symbol-name)
		     collect (symbol-name sym)))
	    (:export ,@symbol-list)))))
