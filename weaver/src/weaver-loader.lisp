;;; -*- Mode: Lisp; Package: weaver -*-
;;; -*- coding:utf-8 -*-

(in-package :WEAVER)

;(defconstant +AspectClass+ "weaver.Aspect")
;(defconstant +PointcutClass+ "weaver.Pointcut")
;(defconstant +JoinpointClass+ "Weaver.Joinpoint")

(defconstant +AspectClass+ "example.jana.classes.Aspect")

(defun load-aspect (class-name)
  (let ((aspect (cl-jana:java-class class-name)))
    (unless
	;(find-annotation (class-annotations aspect) +AspectClass+)
	#?aspect/class-annotations/name[qualified-name=+AspectClass+]
	(error "The jana class '~A' is not a valid Aspect! ~%Please check the annotations." class-name))
    ))

; (weaver::load-aspect "example.jana.classes.TestAspect")

(defun load-target-base-code (class-name-list java-memoization-table)
  "Calclulates the transitive closure over the classes used by the files in
 class-name-list"
  (let ((base-code-classes ()))
    (dolist (class-name class-name-list)
      (push (cl-jana:java-class class-name) base-code-classes))
    (load-undeclared-object-types java-memoization-table)
    (nreverse base-code-classes)))

;(defun load-used-object-types (java-memoization-table)
;  #'(lambda (key val)
;  (subtypep (types java-memoization-table) 'java-object-reference-type)))

	    

;(defun find-annotation (aspect class-name-string)
;; (#${aspect}/[equal qualified-name {class-name-string}]/[= class-annotations {+AspectClass+}]))

;(path-query aspect/[@qualified-name = {class-name-string}]/[@class-annotations = {+AspectClass+})

;; (query (aspect :/ (:@ qualified-name := class-name-string) :/ (:@ class-annotations := +AspectClass+)))

(defun find-annotation (list class-name-string)
  (let ((annotation nil))
    (dolist (elt list)
      (when (string= (cl-jana::qualified-name elt) class-name-string)
	(setq annotation elt)
	(return)))
    annotation))
