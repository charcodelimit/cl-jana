;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.metamodel; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        loader.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Sep 10 09:59:20 2008 (z)
;;; 
;;; Last-Updated: Mi Jan  6 18:29:31 2010 (+0100)
;;;           By: Christian Hofmann
;;; 
;;; Copyright (C) 2008-2009, Christian Hofmann. All rights reserved.
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

(in-package :JANA.METAMODEL)

#.(declaim (inline load-jana-metamodel))
(defun load-jana-metamodel (filename &key (package (package-name cl:*package*)))
  "RETURNS a list containing the jana-metamodel
read in from the file named FILENAME.
The keyword :package can be used to control in which package the symbols
read from the file can be found or will be created.
The default value if the keyword :package is not used is
\(package-name cl:*package*\)."
  (let ((in-pathname (merge-pathnames filename))
	(file-contents ())
        (cl:*package* (find-package package)))
    (declare (type list file-contents))
    (with-open-file (input-stream in-pathname :direction :input)
      (setq file-contents (read input-stream)))
    file-contents))