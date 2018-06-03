;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10.; Package: UT -*-
;;;_____________________________________________________________________________
;;;
;;;                       System: Common Lisp Utilities
;;;                       Module: copy objects
;;;                       Version: 1.0
;;;
;;; Copyright (c): Forschungsgruppe DRUID, Juergen Herczeg
;;;                Universitaet Stuttgart
;;;
;;; File: /usr/local/lisp/xit/cl-utilities/copy-objects.lisp
;;; File Creation Date: 03/19/92 13:58:02
;;; Last Modification Time: 09/18/92 14:33:57
;;; Last Modification By: Juergen Herczeg
;;;
;;;
;;; Changes (worth to be mentioned):
;;; ================================
;;;
;;; chr: - added function delete-all (27-12-2008)
;;;      - added temporary package definitions that allows referring to
;;;        mop features in a uniform way accross different
;;;        CL-Implementations (31-12-2008)
;;;_____________________________________________________________________________

(in-package :util.clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; create the temporary CLOS package and import all needed symbols
  (make-temp-package "CL-UTILITIES-CLOS-MOP"
		     '(slot-definition-allocation
		       slot-definition-name
		       class-slots
		       metaobject))
  ;; export the interface to copy-object
  (export '(instance-slot-names
	    make-uninitialized-instance
	    uninitialized-copy
	    copy
	    shallow-copy
	    deep-copy)))

(defun delete-all (items list)
  "deletes all elements in list ITEMS from the list LIST
and RETURNS the resulting list.
delete-all may modify the contents of LIST."
  (when items
    ;(format t "~%deleting ~A from ~A" items list)
    (map 'list #'(lambda (item)
		   (setq list (delete item list)))
	 items))
  list)

(defmethod instance-slot-names ((object standard-object))
  (let* ((slots (cl-utilities-clos-mop:class-slots (class-of object)))
	 (instance-slots (remove :class slots
				 :key #'cl-utilities-clos-mop:slot-definition-allocation
				 :test #'eq)))
    (mapcar #'cl-utilities-clos-mop:slot-definition-name instance-slots)))

(defmethod make-uninitialized-instance ((class symbol) &rest initargs)
  (apply #'make-uninitialized-instance (find-class class) initargs))

(defmethod make-uninitialized-instance ((class standard-class)
					&rest initargs)
  (declare (ignore initargs))
  (allocate-instance class))

(defmethod shallow-copy (object &key &allow-other-keys)
  ;; primitive objects are not copied
  object)

(defmethod deep-copy (object &key &allow-other-keys)
  ;; primitive objects are not copied
  object)

(defmethod copy (object &key &allow-other-keys)
  (deep-copy object))

#-lucid
;; class metobject is not supplied by lucid clos
(defmethod uninitialized-copy ((object cl-utilities-clos-mop:metaobject) &key &allow-other-keys)
  ;; metaobjects are not copied
  object)

(defmethod shallow-copy ((object sequence) &key &allow-other-keys)
  (copy-seq object))

(defmethod deep-copy ((object list) &key &allow-other-keys)
  (map 'list #'copy object))

(defmethod copy ((object sequence) &key &allow-other-keys)
  ;; sequences are not deep copied by default
  (shallow-copy object))

(defmethod slots-for-identity ((object standard-object))
  ())

(defmethod slots-for-shallow-copy ((object standard-object))
  ())

(defmethod slots-for-deep-copy ((object standard-object))
  ())

(defmethod slots-for-copy ((object standard-object))
  (instance-slot-names object))

(defmethod copy-slot ((from-object standard-object)
		      (to-object standard-object)
		      slot &optional (copy-function #'copy))
  ;(format t "~%Copying slot: ~A" slot)
  (when (slot-boundp from-object slot)
    (setf (slot-value to-object slot)
	(funcall copy-function (slot-value from-object slot)))))

(defmethod copy-slots ((from-object standard-object)
		       (to-object standard-object)
		       &key discard-slots)
  (dolist (slot (delete-all discard-slots
			    (slots-for-identity from-object)))
      (copy-slot from-object to-object slot #'identity))
  (dolist (slot (delete-all discard-slots
			    (slots-for-copy from-object)))
      (copy-slot from-object to-object slot #'copy))
  (dolist (slot (delete-all discard-slots
			    (slots-for-shallow-copy from-object)))
      (copy-slot from-object to-object slot #'shallow-copy))
  (dolist (slot (delete-all discard-slots
			    (slots-for-deep-copy from-object)))
      (copy-slot from-object to-object slot #'deep-copy)))

(defmethod object-creation-function ((object standard-object))
  #'make-uninitialized-instance)

(defmethod after-initialization ((old-object standard-object)
				 (new-object standard-object))
  nil)

(defmethod before-initialization ((old-object standard-object)
				  (new-object standard-object))
  nil)

(defmethod copy :around ((object standard-object)
			 &key discard-slots &allow-other-keys)
  (declare (special *copy-reference-list*))
  (if (boundp '*copy-reference-list*)
      (or (cdr (assoc object *copy-reference-list* :test #'eq))
	  (let ((copy (uninitialized-copy object)))
	    (unless (eq object copy)
	      (setq *copy-reference-list*
		  (acons object copy *copy-reference-list*))
	      (call-next-method object :use-object copy
				       :discard-slots discard-slots))
	    copy))
    (let ((*copy-reference-list* nil))
      (declare (special *copy-reference-list*))
      (let ((copy (uninitialized-copy object)))
	(unless (eq object copy)
	  (setq *copy-reference-list*
	      (acons object copy *copy-reference-list*))
	  (call-next-method object :use-object copy
				   :discard-slots discard-slots))
	copy))))

(defmethod uninitialized-copy ((object standard-object) &key &allow-other-keys)
  (let ((class-name (class-name (class-of object)))
	(creation-function (object-creation-function object)))
    (funcall creation-function class-name)))
	   
(defmethod deep-copy ((object standard-object) &key discard-slots use-object)
  (let ((new-object (or use-object (uninitialized-copy object))))
    (unless (eq new-object object)
      (before-initialization object new-object)
      (copy-slots object new-object :discard-slots discard-slots)
      (after-initialization object new-object))
    new-object))
	   
(defmethod shallow-copy ((object standard-object)
			 &key discard-slots use-object)
  (let ((new-object (or use-object (uninitialized-copy object))))
    (unless (eq new-object object)
      (dolist (slot (delete-all discard-slots (instance-slot-names object)))
	(copy-slot object new-object slot #'identity)))
    new-object))
	   
(defmethod copy ((object standard-object) &key discard-slots use-object)
  (deep-copy object :discard-slots discard-slots
	            :use-object use-object))

(defmethod get-copy (object)
  (declare (special *copy-reference-list*))
  (and (boundp '*copy-reference-list*)
       (cdr (assoc object *copy-reference-list* :test #'eq))))
