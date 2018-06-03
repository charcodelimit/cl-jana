;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: cl-tools; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        package-tools.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;;   Functions to determine elements defined in a package.
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Tue Jun 16 15:19:41 2009 (z)
;;; 
;;; Last-Updated: Mon Aug 24 14:04:15 2009 (z)
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

(in-package :CL-TOOLS)

(defun external-symbols (package-name)
  "RETURNS a list of all symbols external to the package PACKAGE-NAME."
  (let ((ext-sym ()))
    (do-external-symbols (sym (find-package package-name))
      (push sym ext-sym))
    (nreverse ext-sym)))

(defun inherited-symbols (package-name)
  "RETURNS a list of all symbols inherited by the package PACKAGE-NAME."
  (let ((syms-used ()))
    (dolist (pkg (package-use-list (find-package package-name)))
      (when pkg (setq syms-used (append syms-used (external-symbols pkg)))))
    syms-used))

(defun defined-symbols (package-name)
  "RETURNS a list of all symbols in the package PACKAGE-NAME
that were not inherited from other packages,
and thus defined in the package."
  (let ((syms ()))
    (declare (type list syms))
    (do-symbols (sym (find-package package-name))
      (push sym syms))
    (set-difference (nreverse syms) (inherited-symbols package-name))))

(defun internal-symbols (package-name)
  "RETURNS a list of all symbols in a package
that are internal."
  (set-difference (defined-symbols package-name)
                  (external-symbols package-name)))

(defun defined-classes (package-name)
  "RETURNS the symbols of all classes defined in the package."
  (remove-if-not #'(lambda (sym) (find-class sym nil))
                 (defined-symbols package-name)))

(defun external-classes (package-name)
  "RETURNS the symbols of all classes defined in the package."
  (remove-if-not #'(lambda (sym) (find-class sym nil))
                 (external-symbols package-name)))


;; chr: could use Pascal Costanza's closer-mop to make this portable
(defun defined-readers (package-name)
  "RETURNS the symbols of all readers defined in the package."
  #-CLISP (declare (ignore package-name))
  (let (#+CLISP (class-symbols (defined-classes package-name))
        (readers ()))
    #+CLISP
    (dolist (class-sym class-symbols)
      (dolist (slot (clos:class-direct-slots (find-class class-sym)))
        (dolist (reader (clos:slot-definition-readers slot))
          (push reader readers))))
    (setq readers
          (remove-duplicates readers))
    (nreverse readers)))

(defun print-symbols (symbol-list &key test sort-by-name)
  "PRINTS all symbols in SYMBOL-LIST that are matched
by the predicate TEST."
  (let ((syms ()))
    (declare (type list syms))    
    (if test
        (setq syms
              (remove-if-not test symbol-list))
        (setq syms
              symbol-list))
    (when sort-by-name
      (setq syms
            (sort syms #'(lambda (x y) (string< (symbol-name x) (symbol-name y))))))
    (format t "~{~(~A~) ~}" syms)))

(defun print-uninterned-symbols (symbol-list &key test sort-by-name)
  "PRINTS the symbol-names of all symbols in SYMBOL-LIST matched
by the predicate TEST, such that they can be read back in
as uninterned symbols."
  (let ((syms ()))
    (declare (type list syms))
    (if test
        (setq syms
              (remove-if-not test symbol-list))
        (setq syms
              symbol-list))
    (when sort-by-name
      (setq syms
            (sort syms #'(lambda (x y) (string< (symbol-name x) (symbol-name y))))))
    (format t "~{~(#:~A~)~%~}" syms)))

(defun print-defined-symbols (package-name)
  "Prints the symbol-names of all symbols defined
in package PACKAGE-NAME."
  (print-symbols (defined-symbols package-name)))

(defun print-internal-symbols (package-name)
  "Prints the symbol-names of all symbols internal
to package PACKAGE-NAME."
  (print-symbols (internal-symbols package-name)))

(defun print-external-symbols (package-name)
  "Prints the symbol-names of all symbols defined
in package PACKAGE-NAME."
  (print-symbols (external-symbols package-name)))


(defun print-defined-functions (package-name &key sort-by-name)
  "Prints all functions defined in the package PACKAGE-NAME
that are not generic functions."
  (print-symbols
   (defined-symbols package-name)
   :test #'(lambda (sym) (and (fboundp sym)
                              (typep (symbol-function sym) 'function)
                              (not (typep (symbol-function sym) 'generic-function))))
   :sort-by-name sort-by-name))

(defun print-internal-functions (package-name &key sort-by-name)
  "Prints all functions internal to the package PACKAGE-NAME
that are not generic functions."
  (print-symbols
   (internal-symbols package-name)
   :test #'(lambda (sym) (and (fboundp sym)
                              (typep (symbol-function sym) 'function)
                              (not (typep (symbol-function sym) 'generic-function))))
   :sort-by-name sort-by-name))

(defun print-external-functions (package-name &key sort-by-name)
  "Prints all functions whose symbols are exported by the package PACKAGE-NAME,
and that are not generic functions."
  (print-symbols
   (external-symbols package-name)
   :test #'(lambda (sym) (and (fboundp sym)
                              (typep (symbol-function sym) 'function)
                              (not (typep (symbol-function sym) 'generic-function))))
   :sort-by-name sort-by-name))

(defun print-defined-methods (package-name &key sort-by-name)
  "Prints all generic-functions defined in the package PACKAGE-NAME.
Ignores setf methods."
  (print-symbols
   (defined-symbols package-name)
   :test #'(lambda (sym) (and (fboundp sym)
                              (typep (symbol-function sym) 'generic-function)
                              (not (> (mismatch "(setf" (symbol-name sym)) 0))))
   :sort-by-name sort-by-name))


(defun print-internal-methods (package-name &key sort-by-name)
  "Prints all generic-functions internal to the package PACKAGE-NAME.
Ignores setf methods"
  (print-symbols
   (internal-symbols package-name)
   :test #'(lambda (sym) (and (fboundp sym)
                              (typep (symbol-function sym) 'generic-function)
                              (not (> (mismatch "(setf" (symbol-name sym)) 0))))
   :sort-by-name sort-by-name))

(defun print-external-methods (package-name &key sort-by-name)
  "Prints all generic-functions whose symbols are exported by the package PACKAGE-NAME.
Ignores setf methods."
  (print-symbols
   (external-symbols package-name)
   :test #'(lambda (sym) (and (fboundp sym)
                              (typep (symbol-function sym) 'generic-function)
                              (not (> (mismatch "(setf" (symbol-name sym)) 0))))
   :sort-by-name sort-by-name))


(defun print-defined-classes (package-name &key sort-by-name)
  "Prints the symbol-names of all classes defined
in the package PACKAGE-NAME."
  (print-symbols
   (defined-classes package-name)
   :sort-by-name sort-by-name))
   
(defun print-internal-classes (package-name &key sort-by-name)
  "Prints the symbol-names of all classes whose symbol
is internal to the package PACKAGE-NAME."
  (print-symbols
   (set-difference (defined-classes package-name)
                   (external-symbols package-name))
   :sort-by-name sort-by-name))

(defun print-external-classes (package-name &key sort-by-name)
  "Prints the symbol-names of all classes whose symbols are
exported by the package PACKAGE-NAME."
  (print-symbols
   (external-classes package-name)
   :sort-by-name sort-by-name))

(defun print-defined-readers (package-name &key sort-by-name)
  "Prints the symbol-names of all slot-readers defined
in package PACKAGE-NAME."
  (print-uninterned-symbols
   (defined-readers package-name)
   :sort-by-name sort-by-name))

(defun print-internal-readers (package-name &key sort-by-name)
  "Prints all slot-readers defined in package PACKAGE-NAME,
 hose symbol is not exported, which have not the same name
as a class in the package, and which have not the same name
as some other inherited symbol."
  (let ((class-symbols (defined-classes package-name))
        (inherited-symbols (inherited-symbols package-name)))
    (print-uninterned-symbols
     (set-difference (defined-readers package-name)
                     (external-symbols package-name))
     :test #'(lambda (sym)
               (not (and  (member sym class-symbols)
                          (member sym inherited-symbols))))
     :sort-by-name sort-by-name)))

(defun print-external-readers (package-name &key sort-by-name)
  "Prints all slot-readers defined in package PACKAGE-NAME,
 hose symbol is not exported, which have not the same name
as a class in the package, and which have not the same name
as some other inherited symbol."
  (let ((class-symbols (defined-classes package-name))
        (inherited-symbols (inherited-symbols package-name)))
    (print-uninterned-symbols
     (intersection (defined-readers package-name)
                   (external-symbols package-name))
     :test #'(lambda (sym)
               (not (and  (member sym class-symbols)
                          (member sym inherited-symbols))))
     :sort-by-name sort-by-name)))



(export '(print-defined-symbols
          print-external-symbols
          print-internal-symbols
          print-defined-funcions
          print-external-functions
          print-internal-functions
          print-defined-classes
          print-external-classes
          print-internal-classes
          print-defined-methods
          print-external-methods
          print-internal-methods
          print-defined-readers
          print-external-readers
          print-internal-readers))