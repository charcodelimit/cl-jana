;;;-*- Mode: LISP; Syntax: Common-lisp; Package: jana Base: 10 -*-
;;; -*- coding:utf-8 -*-
;;;
;;; Einf"ugen
;;; --------
;;; Fall (1) leerer Wurzelknoten
;;;  f"uge Schl"ussel-Wert Paar mit Wert und Schl"ussel in die Liste
;;;  des Wurzelknotens ein
;;; Fall (2) Knoten mit Schl"ussel-Wert-Paar Liste (kurz: Liste)
;;;  ermittele den Integerwert f"ur den ersten Buchstaben des Schl"ussels
;;;  suche in der Liste ein Schl"ussel-Wert-Paar dessen Schl"ussel
;;;  den selben Integerwert f"ur den ersten Buchstaben hat
;;;  ist ein passender Schl"ussel gefunden?
;;;  falls ja Fall 4
;;;  falls nein Fall 3
;;; Fall (3) kein passendes Schl"ussel-Wert Paar
;;;  h"ange an die Liste ein neues Schl"ussel Wert Paar mit
;;;  Wert und Schl"ussel an
;;; Fall (4) passendes Schl"ussel-Wert Paar gefunden
;;;   ist der gesuchte Schl"ussel gleich dem Schl"ussel des gefundenen
;;;   Schl"ussel-Wert Paars?
;;;   falls ja Fall 6
;;;   falls nein Fall 5

(in-package #:COMMON-LISP-USER)

(defpackage #:JANA
    (:use "COMMON-LISP")
  (:export "MAKE-RTREE"
	   "RTREE-INSERT-ITEM"
	   "RTREE-SEARCH-ITEM"))

(in-package #:JANA)


(defstruct key-value-pair
  key 
  value)

(defmethod print-object ((self key-value-pair) stream)
  (format stream "{~A,~A}"
	  (key-value-pair-key self)
	  (key-value-pair-value self)))

(defmacro key-value-func (key)
  "How to determine the key's value"
  `(char-code (char ,key 0)))

(defmacro key-func (key-value-pair)
  "How to get the key from an item in a list"
  `(key-value-pair-key ,key-value-pair))

#.(declaim (inline key-comparison-fun))
(defun key-comparison-fun (key-value-pair-1 key-value-pair-2)
  (< (key-value-func (key-func key-value-pair-1))
     (key-value-func (key-func key-value-pair-2))))

(declaim (inline binary-search))
(defun binary-search (list char-value)
  (declare  (type list list)
	    (type fixnum char-value))
  (let ((low 0)
	(high (- (length list) 1))
	(middle 0)
	(key 0)
	(element))
    (declare (type fixnum low middle high key))
    (loop
	:while
	(<= low high)
        :do
	(setq middle
	      (ash (+ low high) -1))
	(setq element
	      (nth middle list))
	(setq key
	      (key-value-func (key-func element)))
	(cond ((> key char-value)
	       (setq high (- middle 1)))
	      ((< key char-value)
	       (setq low (+ middle 1)))
	      (t (return element))))))

;;; -- rtree --

(defstruct rtree-root-node
  "The root-node of an r-tree contains
a list of key-value-pairs."
  list)

(defstruct rtree-node
  "The inner nodes of an r-tree can store a value
and a list other key-value-pairs."
  list
  value)

(defmethod print-object ((self rtree-root-node) stream)
  (format stream "[Root] ~A "
	  (rtree-root-node-list self)))

(defmethod print-object ((self rtree-node) stream)
  (format stream "[~A] ~A"
	  (rtree-node-value self)
	  (rtree-node-list self)))

(defun make-rtree ()
  "Creates a root-node instance for an r-tree."
  (make-rtree-root-node))

(defun make-rtree-node-with (node-value key value)
  "Creates a new rtree-node with the value NODE-VALUE
and a new key-value-pair with KEY and VALUE."
  (make-rtree-node :value node-value
		   :list (list
			  (make-key-value-pair :key key
					       :value value))))

(defun rtree-search-item (root-node search-key)
  "Looks up the search key in the r-tree rooted in root-node."
  (if (zerop (length (rtree-root-node-list root-node)))
      NIL ;; CASE (1) empty tree
      (lookup-search-item ;; CASE (2) key-value-pair list
       (rtree-root-node-list root-node)
       search-key)))

(defun lookup-search-item (list search-key)
  "Try to find a matching entry in the key-value-pair list."
  (let* ((key
	  (char-code (char string-key 0)))
	 (value (binary-search list key)))
    (if value
	(key-value-pair-search-item value search-key)
	NIL)))

(defun key-value-pair-search-item (key-value-pair search-key)
  "If the value of the key-value pair is a node, continue the
search there. Otherwise return the value."
  (declare (type key-value-pair key-value-pair)
	   (type string search-key))
  (let* ((key
	  (key-value-pair-key key-value-pair))
	 (value
	  (key-value-pair-value key-value-pair))
	 (index
	  (string/= key search-key)))
    (if (typep value 'rtree-node)
	(if (not index)
	    (rtree-node-value value) 
	    (lookup-search-item (rtree-node-list value)
				(subseq search-key index)))
	(if (not index)
            ; should never happen if key-value-pair-search item
	    ; is not called directly
	    value
	    NIL))))

;;;;;;;;;;;;;; INSERTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro root-node-list-length (rtree-root-node)
  `(length (rtree-root-node-list ,rtree-root-node)))

(defun rtree-insert-item (node key new-value)
  "inserts an item into the root-node by:
\(a\) creating a new list of key-value-pairs, or
\(b\) inserting into the list of key-value-pairs."
  (declare (type rtree-root-node node)
	   (type string key))
  (if (zerop (root-node-list-length node))
      (setf (rtree-root-node-list node) ;; CASE (1) empty tree
	    (list
	     (make-key-value-pair :key key :value new-value)))
      (rtree-root-node-list-insert-item node key new-value)))


(defun rtree-root-node-list-insert-item (node new-key new-value)
  "inserts an item into the list of key-value-pairs associated with
the root-node of an rtree.
If a key-value-pair is found that is associated with the first character
 of the key, then insert into the key-value pair.
Otherwise a new key-value-pair with the new-key and new-value is added
to the list."
  (declare (type rtree-root-node node)
	   (type string new-key))
  (let ((key-value-pair
	 (binary-search (rtree-root-node-list node)
			(key-value-func new-key))))
    (if key-value-pair
	(key-value-pair-insert-item key-value-pair new-key new-value)
	(progn
	  ; insert new key-value-pair
	  (setf (rtree-root-node-list node)
		(push (make-key-value-pair :key new-key :value new-value)
		      (rtree-root-node-list node)))
	  ; sort
	  (setf (rtree-root-node-list node)
		(sort (rtree-root-node-list node)
		      #'key-comparison-fun))))))

(defun key-value-pair-insert-item (key-value-pair new-key new-value)
  "A key-value-pair's key can match the new-key, in which case the
value is replaced.
Otherwise the key-value-pair's value needs to be updated."
  (declare (type key-value-pair key-value-pair)
	   (type string new-key))
  (let ((key
	 (key-value-pair-key key-value-pair)))
    (declare (type string key))
    (if (string= key new-key)
	(key-value-pair-replace-value key-value-pair new-value)
	(key-value-pair-update-value key-value-pair new-key new-value))))

(defmacro rtree-node-set-value (node value)
  `(setf (rtree-node-value ,node)
         ,value))
  
(defun key-value-pair-replace-value (key-value-pair new-value)
  "The value of a key-value-pair can be a data-item or
an rtree-node.
If the value is an rtree-node update the rtree-node's value slot.
Otherwise replace the data-item."
  (declare (type key-value-pair key-value-pair))
  (if (typep (key-value-pair-value key-value-pair)
	     'rtree-node)
      (rtree-node-set-value (key-value-pair-value key-value-pair)
			    new-value)
      (setf (key-value-pair-value key-value-pair)
	    new-value)))

(defmacro real-prefix-p (string-1 string-2)
  `(and
    (< (length ,string-1) (length ,string-2))
    (= (string/= ,string-1 ,string-2)
       (length ,string-1))))

(defun key-value-pair-update-value (key-value-pair new-key new-value)
  "When the key-value-pair's key is smaller than the new-key,
   then is the key a prefix of the new-key.
   Otherwise the new-key has to be used as the new prefix."
  (declare (type key-value-pair key-value-pair)
	   (type string new-key))
  (let* ((key
	  (key-value-pair-key key-value-pair))
	 (mismatch-index (string/= key new-key)))
    (declare (type string key)
	     (type fixnum mismatch-index))
    (assert (/= mismatch-index 0) () "The keys are the same!
Their value should be replaced not updated!")
    (cond
      ((= (length key) mismatch-index) ; echter Pr"afix
       (key-value-pair-update-postfix key-value-pair
				      (subseq new-key mismatch-index)
				      new-value))
      ((= (length new-key) mismatch-index) ; echter Postfix
       (key-value-pair-update-prefix key-value-pair
				     (subseq key mismatch-index)
				     new-key
				     new-value))
      (t ; kein echter Pr"a- oder Postfix
       (key-value-pair-update key-value-pair
			      (subseq key 0 mismatch-index)
			      (subseq key mismatch-index)
			      (subseq new-key mismatch-index)
			      new-value)))))

(defun key-value-pair-update-postfix (key-value-pair postfix new-value)
  "Insert a new postfix into a key-value-pair.
When no rtree-node exists, create a new node with the current
key-value-pair's value and  a new key-value-pair."
  (declare (type key-value-pair key-value-pair)
	   (type string postfix))
  (let ((value
	 (key-value-pair-value key-value-pair)))
    (if (typep value 'rtree-node)
	; insert item into rtree-node
	(rtree-node-insert-item value postfix new-value)
	; replace the key-value-pair's value with a new rtree-node
	(setf (key-value-pair-value key-value-pair)
	      (make-rtree-node-with value postfix new-value)))))


(defun key-value-pair-update-prefix (key-value-pair postfix prefix new-value)
  "Insert a new prefix into a key-value-pair.
This requires replacement of the current key-value-pair's
key and value and inserting it as a new key-value-pair in the
list of the rtree-node."
  (declare (type key-value-pair key-value-pair)
	   (type string postfix prefix))
  (let ((value
	 (key-value-pair-value key-value-pair)))
    (if (typep value 'rtree-node)
	(progn
	  ; update key of the key-value-pair
	  (rtree-node-insert-item value postfix (rtree-node-value value))
	  (setf (key-value-pair-key key-value-pair)
		prefix) 
	  ; update value of the rtree-node
          (setf (rtree-node-value value)
		new-value)) 
        (progn
	  ; update key
	  (setf (key-value-pair-key key-value-pair)
		prefix)
	  ; update value of key-value-pair
	  (setf (key-value-pair-value key-value-pair)
		(make-rtree-node-with new-value postfix value))))))

(defun key-value-pair-update (key-value-pair common-prefix
					     postfix-key postfix-new-key
					     new-value)
  "Insert a new common-prefix and add the old and new key's and values
to the key-value-pair list of the rtree-node."
  (declare (type key-value-pair key-value-pair)
	   (type string common-prefix postfix-new-key postfix-new-key))
  (let ((value
	 (key-value-pair-value key-value-pair)))
    (if (typep value 'rtree-node)
	(progn
	  ; update key of the key-value-pair
	  (setf (key-value-pair-key key-value-pair)
		common-prefix)
	  ; insert new node with the values of the current key-value-pair
	  ; as the first key-value-pair in its list
	  ; (push-key-value-pair-down)
	  (setf (key-value-pair-value key-value-pair)
		(make-rtree-node-with nil
				      postfix-key
				      value))
	  ; insert new-value
	  (rtree-node-insert-item (key-value-pair-value key-value-pair)
				  postfix-new-key
				  new-value))
	(progn
	  ; update key
	  (setf (key-value-pair-key key-value-pair)
		common-prefix)
	  ; update value of key-value-pair
	  (setf (key-value-pair-value key-value-pair)
		(make-rtree-node-with nil postfix-key value))
	  ; insert new value
	  (rtree-node-insert-item (key-value-pair-value key-value-pair)
				  postfix-new-key
				  new-value)))))
   
	  
(defun rtree-node-insert-item (node new-key new-value)
  "Search a matching key-value-pair in the node's list.
If none is found insert a new key-value-pair into the list."
  (declare (type rtree-node node)
	   (type string new-key))
  (assert (> (length (rtree-node-list node)) 0)
	  ()
	  "rtree-nodes should always be created using make-rtree-node-with!")
  (let ((key-value-pair
	 (binary-search (rtree-node-list node)
			(key-value-func new-key))))
    (if key-value-pair
	(key-value-pair-insert-item key-value-pair new-key new-value)
	(setf (rtree-node-list node)
	      (progn
		; insert new key-value-pair
		(push (make-key-value-pair :key new-key :value new-value)
		      (rtree-node-list node))
		; sort
		(setf (rtree-node-list node)
		      (sort (rtree-node-list node)
			    #'key-comparison-fun)))))))

(defun test-r-tree ()
  "A simple sanity check. It checks whether find and insert
 work correctly together. However, it does not
guarantee that the structure of the tree is correct."
  (let ((test-r-tree (make-rtree)))
    (rtree-insert-item test-r-tree "rom" 'rom)
    (assert
     (eq (rtree-search-item test-r-tree "rom")
	 'rom))
    (rtree-insert-item test-r-tree "rom" 'imperium-romanum)
    (assert
     (eq (rtree-search-item test-r-tree "rom")
	 'imperium-romanum))
    (rtree-insert-item test-r-tree "romanum" 'romanum)
    (assert
     (eq (rtree-search-item test-r-tree "romanum")
	 'romanum))
    (rtree-insert-item test-r-tree "rom" 'rom)
    (assert
     (eq (rtree-search-item test-r-tree "rom")
	 'rom))
    (rtree-insert-item test-r-tree "romanae" 'romanae)
    (assert
     (eq (rtree-search-item test-r-tree "romanae")
	 'romanae))
    (rtree-insert-item test-r-tree "romanus" 'romanus)
    (assert
     (eq (rtree-search-item test-r-tree "romanus")
	 'romanus))
    (rtree-insert-item test-r-tree "romae" 'romae)
    (assert
     (eq (rtree-search-item test-r-tree "romae")
	 'romae))))
