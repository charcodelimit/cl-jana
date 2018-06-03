;;; -*- coding:utf-8 -*-


(in-package #:COMMON-LISP-USER)

(defpackage "WEAVER"
  ; (:nicknames "WEAVER")
  (:use "COMMON-LISP")
  ;;; cat *.lisp | sed -n -e '/^(DEF/s/^(DEF/"/p'
  (:export
   "LOAD-ASPECT")
  (:documentation
   "Christian Hofmann 2008 - 2008
    This package is provided under the GNU General Public License.
    See the source file for details."))