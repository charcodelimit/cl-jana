;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        weave-eraser-aspect.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Mon Jul  6 15:29:46 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:57 2010 (+0100)
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

(in-package :JANA.MX.JAVA.JIMPLE)

(defvar *monitor-aspect-class-name*)
;(setq *monitor-aspect-class-name* "rtv.lockset.monitor.NaiveLockSetMonitor")
;(setq *monitor-aspect-class-name* "rtv.lockset.monitor.OriginalEraserMonitor")
;(setq *monitor-aspect-class-name* "rtv.lockset.monitor.RacerMonitor")
(setq *monitor-aspect-class-name* "rtv.lockset.monitor.ConservativeMonitor")

(defvar *use-static-analysis*)
(setq *use-static-analysis* t)

(defun make-log-field-read-reference ()
  "Creates a method-reference to the Java method:
public static synchronized void logFieldRead(Object fieldOwnerInstance, String fieldOwnerClass, String aFieldName, String methodSignature)."
  (jimple-reference-value-method "logFieldRead"
   (java-type "void")
   (list
     (java-type "java.lang.Object")
     (java-type "java.lang.String")
     (java-type "java.lang.String")
     (java-type "java.lang.String"))))

(defun make-log-field-write-reference ()
  "Creates a method-reference to the Java method:
public static synchronized void logFieldWrite(Object fieldOwnerInstance, String fieldOwnerClass, String aFieldName, String methodSignature)."
  (jimple-reference-value-method "logFieldWrite"
   (java-type "void")
   (list
     (java-type "java.lang.Object")
     (java-type "java.lang.String")
     (java-type "java.lang.String")
     (java-type "java.lang.String"))))

(defun make-lock-reference ()
  "Creates a method-reference to the Java method:
public static synchronized void lock(Object lockInstance)."
  (jimple-reference-value-method "lock"
   (java-type "void")
   (list
    (java-type "java.lang.Object"))))

(defun make-unlock-reference ()
  "Creates a method-reference to the Java method:
public static synchronized void unlock()."
  (jimple-reference-value-method "unlock"
   (java-type "void")
   '()))

;; chr: this is the correct way to do it,
;;      Just iterating the hash-values in the class-name map
;;      might not work, as more classes may have been loaded
;;      (and thus in (classes project))  than only those that
;;      belong to the project (class-names project)
(defmethod collect-classes ((self java-project))
  "RETURNS the classes belonging to the java-project SELF."
  (declare #.*standard-optimize-settings*)
  (let ((class-map (classes self)))
        (declare (type hash-table class-map))
        (loop
            :for class-name :in (class-names self)
            :when (gethash class-name class-map)
              :collect
               (gethash class-name class-map))))

(defun collect-synchronized-methods (classes)
  "Requires path (class . method)"
  (declare #.*standard-optimize-settings*
           (type list classes))
  (let ((tuples '()))
    (declare (type list tuples))
    (dolist (class classes)
      (dolist (method (methods class))
        (when (and
	       (typep method 'java-method-implementation)
	       (has-modifier-p method 'synchronized))
          (push (cons class method) tuples))))
    (nreverse tuples)))

(defun collect-concurrency-control-statements (classes)
  "Requires paths (method . instruction)."
  (declare #.*standard-optimize-settings*
           (type list classes))  
  (let ((body)        
        (tuples '()))
    (declare (type list tuples))
    (dolist (class classes)
      (dolist (method (methods class))
        (when (typep method 'java-method-implementation)
          (setq body (body method))
          (dolist (instruction (instructions body))
            (when (typep instruction 'jimple-conditional-critical-section-instruction)
              (push (cons method instruction) tuples))))))
    (nreverse tuples)))

(defconstant +special-name-constructor+ "<init")

(defun find-super-constructor-call (method-name closure)
  "RETURNS the position of the super-constructor call
in the constructor with name METHOD-NAME and closure CLOSURE.
Only instance initializers, i.e. <init>, may have super-initializers."
  (declare #.*standard-optimize-settings*
           (type string method-name)
           (type jana-closure closure))
  (if (equal method-name +special-name-constructor+) ;; chr: ToDo use constants here
      (let ((this-assignment-instruction)
            (this-reference-value-local))
        (setq this-assignment-instruction
              (nth-value 0 (find-this-assignment closure)))
        (setq this-reference-value-local
              (assignment-target this-assignment-instruction))
        (loop
            :for instruction :in (instructions closure)
            :for position :of-type fixnum :from 0
            :when (and (typep instruction 'jimple-invoke-special-instruction)
                       (equal +special-name-constructor+
                              (method-name (method-reference instruction)))
                       (equalp this-reference-value-local (local-variable instruction)))
              :do (return (values instruction position))
            :finally
              (return (values nil -1))))
      (values nil -1)))

(defun collect-field-access-statements (classes project)
  "Requires list of paths (method . instruction).
class is required for creating the instance-level-context,
method and body for creating the method-level-context."
  (declare #.*standard-optimize-settings*
           (type list classes))  
  (let ((body)
        (tuples '())
        (collect-instance-variables t)
        (method-name "")
        (super-constructor-call-position 0)
        (memoization-table (make-hash-table :test #'equal)))
    (declare (type list tuples)
             (type hash-table memoization-table)
             (type string method-name))
    (dolist (class classes)
      (dolist (method (methods class))
        (when (typep method 'java-method-implementation)
          (setq body (body method))
          (setq method-name
                (unqualified-name method))
          ;; is it a constructor
          (if (and (equal method-name "<init>")
                   *use-static-analysis*)
              ;; only collect instance variables when this escapes              
              (setq collect-instance-variables
                    (this-escape-analysis method class project memoization-table))
              ;; if not -- always collect instance variables
              (setq collect-instance-variables t))
          ;; static initializers are synchronized as defined in the JVM spec.
          (unless (and (equal method-name "<clinit>")
                       *use-static-analysis*)
            ;; if escape analysis is off, collect field accesses in <clinit> too
            ;; otherwise collect in <init>, and all other methods only
            (setq super-constructor-call-position
                  (nth-value 1 (find-super-constructor-call method-name body)))
            (loop
                :for instruction :in (instructions body)
                :for position :of-type fixnum :from 0
                :do
                  (when (and (typep instruction 'jimple-imaginary-instruction-assignment)
                             (> position super-constructor-call-position))
                    ;; access of class variables -- always track
                    (when  (or (typep (assignment-target instruction) 'jimple-reference-value-class-variable)
                               (typep (assignment-source instruction) 'jimple-reference-value-class-variable))
                      ;; when source or target of the assignement was a class-variable then collect                      
                      (push (cons method instruction) tuples))
                    ;; access of instance variables -- only safe in constructors where this does not escape
                    (when  (and collect-instance-variables
                                (or (typep (assignment-target instruction) 'jimple-reference-value-instance-variable)
                                    (typep (assignment-source instruction) 'jimple-reference-value-instance-variable)))
                      ;; when source or target of the assignement was an instance-variable then collect
                      (push (cons method instruction) tuples))))))))
      (nreverse tuples)))


;;; -------------- TRANSFORMATION --------------

(defun full-java-method-signature (method)
  "RETURNS a string representation of the full signature of a java-method."
  (with-output-to-string (stream)
    (jana.java.jimple.conversion:write-full-java-method-signature method stream)))

;; need a copy constructor for the argument-types here, but I will not change types so it is o.k.
;; use this code to create a method call context object
;; java-method-call-context -> java-context
(defun init-local-variables (java-method-implementation)
  "RETURNS a list of local variables, the list of instructions that initialize
the variables, and the local variable that can be used to refer to this."
  (let ((local-variable-declarations)
        (argument-variables ())
	(return-variable nil)
	(this-variable nil)
	(this-type nil)
	(instructions ())
	(count 0)
        (variable-declaration)
        (value-variable))
    (setq this-type
          (if (has-modifier-p java-method-implementation 'static)
              (java-type +class-class-signature+)
              (java-object-reference-type (owner-class java-method-implementation))))
    ;; init local variable for reference to this
    (setq variable-declaration
	  (jimple-local-variable-declaration
	   (concatenate 'string "w$t" (princ-to-string count))
	   this-type))
    (push variable-declaration
          local-variable-declarations)
    (setq this-variable
          (jimple-reference-value-from-declaration variable-declaration))
    (unless (has-modifier-p java-method-implementation 'static)
      (push (jimple-imaginary-instruction-variable-initialization
             this-variable
             (jimple-reference-value-this this-type)
             -1)
            instructions))
    ;; init local variables for passing method arguments
    (dolist (argument-type (argument-types java-method-implementation))
      (setq variable-declaration
	    (jimple-local-variable-declaration
	     (concatenate 'string "w$a" (princ-to-string count))
	     argument-type))
      (push variable-declaration
            local-variable-declarations)
      (setq value-variable
            (jimple-reference-value-from-declaration variable-declaration))
      (push value-variable
	    argument-variables)
      (push (jimple-imaginary-instruction-variable-initialization
	     value-variable
	     (jimple-reference-value-argument count argument-type)
             -1)
	    instructions)
      (incf count))
    (setq argument-variables
          (nreverse argument-variables))
    ;; assign this-class
    (when (has-modifier-p java-method-implementation 'static)    
      (push (jimple-imaginary-instruction-assignment
             this-variable
             (java-constant-class-reference (qualified-name (owner-class java-method-implementation)))
             -1)
            instructions))
    ;; create local variable for return value
    (unless (typep (return-type java-method-implementation) 'java-void-type)
      (setq variable-declaration
	    (jimple-local-variable-declaration
	     (concatenate 'string "w$r" (princ-to-string count))
	     (return-type java-method-implementation)))
      (push variable-declaration
            local-variable-declarations)
      (setq return-variable
            (jimple-reference-value-from-declaration variable-declaration)))
    (values
     local-variable-declarations
     argument-variables
     instructions
     this-variable
     return-variable)))

(defun init-wrapper-context (project class wrapper wrapped-method)
  "Adds the following contexts to the closure of the
java-method declaration WRAPPER:
The method-invocation context for the java-method-declaration WRAPPED-METHOD,
the dynamic closure context for the java-method-declaration WRAPPER,
and the classifier or instance context for the java-classifier-declaration CLASS
in the java-project PROJECT.
RETURNS the closure of the java-method WRAPPER."
  ;;; chr: wrapper has the same signature as the wrapped method,
  ;;; besides the wrapped-method being private and final
  (declare #.*standard-optimize-settings*
           (type java-project project)
           (type java-classifier-declaration class)
           (type java-method-implementation wrapper wrapped-method))
  (let ((wrapper-closure (body wrapper))
        (instance-context)
        (classifier-context)
        (invocation-context)
        (dynamic-closure-context))
    ;; add method-invocation context to the wrapped-method
    (setq invocation-context
          (java-declared-method-invocation-context wrapped-method))          
    (add-exception-variables invocation-context wrapper-closure)
    (add-return-value-variable invocation-context wrapper-closure)
    ;; add dynamic closure context to the wrapper
    (dynamic-closure-context (jimple-closure-context wrapper-closure))
    (add-argument-value-variables dynamic-closure-context wrapper-closure)
    ;; add classifier-context if method is static, and this-context otherwise
    (cond ((has-modifier-p 'static wrapper)
           (setq classifier-context
                 (java-classifier-context class project))
           (add-classifier-context classifier-context wrapper-closure))
          (t
           (setq instance-context (java-instance-context class project))
           (add-this-context instance-context wrapper-closure)))
    ;; return the closure of the java-method-declaration wrapper
    wrapper-closure))


;;; exception handler creation

(defmethod add-exception-handlers ((synchronized-wrapper jimple-closure) wrapped-method-call-label wrapped-method-call-position)
  (let ((var-this (this-context-variable (instance-context synchronized-wrapper))))
    (multiple-value-bind (catch-instructions trap-instructions)
        (make-catch-blocks synchronized-wrapper
                           (method-exception-variables (dynamic-closure-context synchronized-wrapper))
                           `(jimple-ccsec-exit-instruction ,var-this -1)
                           wrapped-method-call-label
                           wrapped-method-call-label)
      ;; insert catch-blocks after wrapped-method-call
      (insert-instructions-after synchronized-wrapper
                                 catch-instructions
                                 (find-instruction-index synchronized-wrapper
                                                         wrapped-method-call-position
                                                         :starting-at wrapped-method-call-position))
      ;; add traps at the end of the instructions list
      (setf (instructions synchronized-wrapper)
            (nconc (instructions synchronized-wrapper)
                   trap-instructions)))))
                       

;;; chr: test!
;;; destructive operations could mess up inserting instructions multiple times
(defun make-catch-blocks (closure method-exception-variables instructions from-label to-label)
  "RETURNS a list of instructions that can be used to catch and
rethrow the exceptions of the java-object-reference-types
stored in the list EXCEPTION-TYPES after executing the
jimple-instructions in the list instructions."
  (declare #.*standard-optimize-settings*
           (type list method-exception-variables instructions))
  (let ((catch-instructions '())
        (catch-block-instructions '())
        (trap-instructions '()))
    (declare (type list catch-instructions catch-block-instructions trap-instructions))
    (dolist (variable-declaration method-exception-variables)
      (setq catch-block-instructions
            (make-catch-block closure
                              instructions
                              (jimple-reference-value-from-declaration
                               variable-declaration)))
      (jimple-imaginary-trap-instruction
       from-label
       to-label
       (branch-target-label (first catch-block-instructions))
       (jana-type variable-declaration))
      (setq catch-instructions
            (nconc catch-instructions
                   catch-block-instructions)))
    (values catch-instructions trap-instructions)))

(defun make-catch-block (closure instructions exception-variable-reference)
    (let ((catch-block-instructions '())
          (first-instruction))
      (declare (type list catch-block-instructions))
      ;; initialize exception-variable with the caught exception
      (setq first-instruction
            (jimple-imaginary-instruction-variable-initialization
             exception-variable-reference
             (jimple-reference-value-caught-exception
              (jana-type exception-variable-reference))
             -1))
      (setf (branch-target-label first-instruction)
            (new-branch-target-label closure))
      (push first-instruction catch-block-instructions)
      ;; instructions list, and rethrow
      (setq catch-block-instructions
            (append catch-block-instructions instructions))
      (setq catch-block-instructions
            (append catch-block-instructions
                    (jimple-throw-instruction exception-variable-reference -1)))
      catch-block-instructions))
  

;;; wrapper creation

(defun create-wrapper (method)
  "RETURNS a wrapper for the java-method-implementation METHOD."
  (declare #.*standard-optimize-settings*
           (type java-method-implementation method))
  (format t "~%[Creating Wrapper] ~A" (full-java-method-signature method))
  (let ((wrapper (copy-java-method-implementation method)))
    (setf (body wrapper)
          (copy-jimple-closure method))
    wrapper))

(defun create-hidden-base-method (method)
  "Creates a new private final method from the java-method METHOD.
RETURNS the new hidden base-method."
  (let ((base-method
         (copy-java-method-implementation
          method
          (concatenate 'string "$Gen$" (copy-seq (qualified-name method)))))
        (method-modifiers))
    (setq method-modifiers
          (method-modifiers base-method))
    (setf (modifier-list method-modifiers)
          (union (set-difference (modifier-list method-modifiers) '(public protected synchronized))
                 '(private final)))
    (setf (body base-method)
          (body method))
    base-method))

(defmethod replace-method-in-class ((self java-classifier-declaration) source-method target-method)
  "Replaces the method SOURCE-METHOD in java-class SELF against the method TARGET-METHOD."
  (if (member source-method (methods self))
      (rplaca (member source-method (methods self))
              target-method)
      (progn
        (format t "~%Searching: ~A~%" (full-java-method-signature source-method))
        (loop :for method :in (methods self)
              :do
              (format t "~%~A" (full-java-method-signature method)))
        (break))))

(defmethod add-synchronization-wrapper ((project java-project) (self java-classifier-declaration) (method java-method-declaration))
  "RETURNS the wrapping-method or NIL when no wrapper was generated."
  (let ((closure)
	(method-modifiers)
	(var-this)
	(var-return-value)
        (base-method nil)        
	(wrapping-method)
	(vlist)
        (current-instruction)
	(invocation-instruction)
	(local-variable-declarations ())
        (argument-variables ())
	(instructions ()))
    ;; init
    (unless (equal (qualified-name self)
                   (qualified-name (owner-class method)))
      (error "Classes differ! ~% Class-Argument: ~A  Owner-class of Method-Argument: ~A"
             (qualified-name self)
             (qualified-name (owner-class method))))
    (setq method-modifiers
          (method-modifiers method))
    (setq wrapping-method nil)
    (setq instructions nil)
    (when (and
           (typep method 'java-method-implementation)
           (has-modifier-p method 'synchronized))
        (format t "~% Transforming Method: ~A" (qualified-name self) (full-java-method-signature method))
        ;; create wrapping method
        (setq wrapping-method
              (create-wrapper method))
        ;; modify signature of wrapped method
        (setq base-method
              (create-hidden-base-method method))
        (replace-method-in-class self method base-method)
        ;; modify signature of wrapping method
        (setq method-modifiers
              (method-modifiers wrapping-method))
        (setf (modifier-list method-modifiers)
              (remove 'synchronized (modifier-list method-modifiers) :test 'equal))
        ;; create body of wrapping method
        (setq vlist
              (multiple-value-list (init-local-variables wrapping-method)))
        (setq local-variable-declarations (first vlist))
        (setq argument-variables (second vlist))
        (setq var-this (fourth vlist))
        (setq var-return-value (fifth vlist))
        (setq instructions
              (third vlist))
        (format t "~%~A" (full-java-method-signature method))
        ;; modify method body of the wrapper method
        (setq closure
              (body wrapping-method))
        (setf (local-variables closure)
              (nreverse local-variable-declarations))
        ;; add wrapping code
        ;; MONITORENTER
        (push (jimple-ccsec-enter-instruction var-this -1)
              instructions)
        (if (has-modifier-p method 'static)
            (setq invocation-instruction
                  (jimple-invoke-static-instruction
                   (java-object-reference-type (owner-class base-method))
                   (jimple-reference-value-from-declaration base-method)
                   argument-variables))
            (setq invocation-instruction
                  (jimple-invoke-virtual-instruction
                   var-this
                   (java-object-reference-type (owner-class base-method))
                   (jimple-reference-value-from-declaration base-method)
                   argument-variables)))
        ;; return of base-method
        ;; (setf (branch-target-label invocation-instruction)
        ;;       (new-branch-target-label closure))
        (if var-return-value
            (setq current-instruction
                  (jimple-imaginary-instruction-assignment
                   var-return-value
                   invocation-instruction
                   -1))
            (setq current-instruction
                  invocation-instruction))
        (setf (branch-target-label current-instruction)
              (new-branch-target-label closure))
        (push current-instruction
              instructions)
        ;; MONITOREXIT
        (setq current-instruction
              (jimple-ccsec-exit-instruction var-this -1))
        (setf (branch-target-label current-instruction)
              (new-branch-target-label closure))
        (push current-instruction
              instructions)
        ;; return of wrapper
        (if var-return-value
            (push (jimple-return-instruction var-return-value -1)
                  instructions)
            (push (jimple-return-void-instruction -1)
                  instructions))
        (setf (instructions closure)
              (nreverse instructions))
        (initialize-branch-target-labels closure)
        (initialize-instructions closure)
        (setf (body wrapping-method)
              closure))
    wrapping-method))


(defmethod add-synchronization-wrappers ((self java-project))
  "Replaces synchronized methods in the java-project SELF
with wrapper methods and synchronized-blocks."
  (jana.java:load-java-classes self) ; load classes
  (let ((class)
	(closure)
	(method-modifiers)
	(var-this)
	(var-return-value)
        (base-method)        
	(wrapping-method)
	(vlist)
	(invocation-instruction)
	(local-variable-declarations ())
        (argument-variables ())
	(instructions ())
	(wrapping-methods ()))
    (dolist (class-name (class-names self)) ; all classes
      (setq class
	    (gethash class-name (classes self)))
      (setq wrapping-methods nil)
      (dolist (method (methods class)) ; all methods
	;; init
	(setq method-modifiers
	      (method-modifiers method))
	(setq wrapping-method nil)
	(setq instructions nil)
	(format t ".")
        (setq wrapping-method (add-synchronization-wrapper self class method))
        (when wrapping-method
          (format t "~%~A" (jana.java.jimple.conversion:jimple-statement wrapping-method))
          (push wrapping-method
                wrapping-methods)))
  (setf (methods class)
        (append (methods class) (nreverse wrapping-methods)))        
  )))

(defmethod weave-eraser-aspect ((self java-project))
  "Adds synchronization wrappers, collects all field-access statements,
and inserts calls of the run-time monitor."
  (let ((classes '())
        (tuples '())
        (array-assignments '())
        (array-field-reference-map (make-hash-table)))
    (declare (type list classes tuples))
    (add-synchronization-wrappers self)
    (setq classes
          (collect-classes self))
    (setq tuples
          (collect-field-access-statements classes self))
    (loop :for tuple :in tuples
          :do (if (array-field-reference-assignment-p tuple)
                  (push tuple array-assignments)
                  (insert-log-field-access-call tuple)))
    (setq array-field-reference-map
          (collect-array-field-references array-assignments))
    (loop :for method :being :the hash-keys :in array-field-reference-map
          :do (insert-log-array-field-access-call method
                                                  (gethash method array-field-reference-map)))
    (setq tuples
          (collect-concurrency-control-statements classes))
    (loop :for tuple :in tuples
          :do (cond ((typep (cdr tuple) 'jimple-ccsec-enter-instruction)
                     (insert-log-ccsec-enter-call tuple))
                    ((typep (cdr tuple) 'jimple-ccsec-exit-instruction)
                     (insert-log-ccsec-exit-call tuple))
                    (t
                     (warn "Expected a jimple-ccsec-enter-instruction or jimple-ccsec-exit-instruction, found ~A instead."
                          (type-of (cdr tuple))))))))

(defun insert-log-array-field-access-call (method field-reference-map)
  (declare #.*standard-optimize-settings*
           (type hash-table field-reference-map))
  (let ((bound-values '())
        (is-write nil)
        (position 0)
        (method-reference)
        (local-reference)
        (field-reference)
        (invocation-instruction))
    (declare (type list bound-values)
             (type boolean is-write)
             (type fixnum position))
  (loop :for instruction :in (instructions (body method))
        :do
          (when (and (typep instruction 'jimple-imaginary-instruction-assignment)
                     (or (typep (assignment-target instruction)
                                'jimple-reference-value-array)
                         (typep (assignment-source instruction)
                                'jimple-reference-value-array)))
            (setq position
                  (instruction-index instruction))
            (cond ((typep (assignment-target instruction)
                          'jimple-reference-value-array)
                   (setq local-reference
                         (local-variable-instance-reference (assignment-target instruction)))
                   (setq is-write t))
                  ((typep (assignment-source instruction)
                          'jimple-reference-value-array)
                   (setq local-reference
                         (local-variable-instance-reference (assignment-source instruction)))
                   (setq is-write nil)))
            (when (gethash (unqualified-name local-reference) field-reference-map)
              (setq field-reference
                    (gethash (unqualified-name local-reference) field-reference-map))
              ;; public static synchronized void
              ;;  logFieldWrite(Object fieldOwnerInstance, String fieldOwnerClass, String aFieldName, String methodSignature)
              (setq bound-values
                    (list
                     :value (if (typep field-reference 'jimple-reference-value-instance-variable)
                                (local-variable-instance-reference field-reference)
                                (java-null-value))
                     :constant (qualified-name (field-owner field-reference))
                     :constant (field-name field-reference)
                     :constant (full-java-method-signature method)))
              (if is-write
                  (setq method-reference
                        (make-log-field-write-reference))
                  (setq method-reference
                        (make-log-field-read-reference)))
              (setq
               invocation-instruction
               (make-static-invocation-instruction method-reference
                                                   *monitor-aspect-class-name*
                                                   bound-values))
              (insert-instructions-before
               (body method)
               (list invocation-instruction)
               (find-instruction-index (body method) position :starting-at position)))))
            (when (and invocation-instruction (debug-mode))
              (format t "~%Transformed Method-Body~%~A"
                      (jana.java.jimple.conversion:jimple-statement (body method))))))
              

(defun collect-array-field-references (tuples)
  "RETURNS a table that maps methods to hash-tables containing the mappings
from local-variable-references to field-references."
  (declare #.*standard-optimize-settings*
           (type list tuples))
   (let ((method)
         (instruction)
         (array-references (make-hash-table :test #'equal))
         (method-table (make-hash-table))
         (local-variable-reference))
     ;; create per-method a table of local-variable names to field-references
     (dolist (tuple tuples method-table)
       (setq method
             (car tuple))
       (setq instruction
             (cdr tuple))
       (if (gethash method method-table)
           (setq array-references
                 (gethash method method-table))             
           (setq array-references
                 (make-hash-table :test #'equal)))
       (setq local-variable-reference
             (assignment-target instruction))
       (setf (gethash (unqualified-name local-variable-reference)
                      array-references)
             (assignment-source instruction))
       (setf (gethash method method-table)
             array-references))))
  
(defun array-field-reference-assignment-p (tuple)
  "RETURNS T if the assignement statement in tuple TUPLE assigns a field
 of type java-array-type."
  (declare #.*standard-optimize-settings*
           (type cons tuple))
   (let ((instruction)
         (reference-value))
     (setq instruction
           (cdr tuple))
     (cond ((typep (assignment-source instruction) 'jimple-reference-value-field)
            (setq reference-value (assignment-source instruction))
            (when (typep (jana-type reference-value) 'java-array-type)
              t))
           (t
            nil))))

#.(declaim (inline constructor-call-p))
(defun constructor-call-p  (instruction)
  "RETURNS T if instruction is a method-invocation-instruction
that calls a constructor. A static initializer, that is <clinit>,
 is never called explicitly from Java virtual machine instructions, 
but only implicitly by the Java virtual machine itself."
  (and 
   (typep instruction 'jimple-invoke-special-instruction)
   (equal (method-name (method-reference instruction))
          "<init>")))

(defun locate-constructor-call (closure &key (max-position -1))
  "RETURNS the instruction index where the first constructor 
call is located in the jimple-closure CLOSURE, or -1 when no
such call has been found. Constructors, that is, instance initialization methods
 may be invoked only within the Java virtual machine by the invokespecial instruction, 
and they may be invoked only on uninitialized class instances."
  (declare #.*standard-optimize-settings*)
  (let ((instruction-count 0))
    (declare (type (unsigned-byte 32) instruction-count))
    (dolist (instruction (instructions closure) -1)
      (cond ((constructor-call-p instruction)
             (return instruction-count))
            ((typep instruction 'jimple-method-invocation-instruction)
             ;; method invocation constructions are only allowed after the constructor call
             (return -1))
            ((and (not (= max-position -1))
                  (> instruction-count max-position))
             (return -1))
            (t
             (incf instruction-count))))))


(defun insert-log-field-access-call (tuple)
  "Inserts a method call to the run-time monitor,
provided a cons TUPLE of the form 
(java-method-implementation . instruction).
The method call is inserted before the instruction."
  (declare #.*standard-optimize-settings*)
  (let ((bound-values '())
        (method)
        (method-reference)
        (invocation-instruction)
        (instruction)
        (reference-value)
        (position 0)
        (constructor-call-position -1)
        (is-write nil))
    (declare (type (unsigned-byte 32) position)
                  (type fixnum constructor-call-position))
    (setq method
          (car tuple))
    (setq instruction
          (cdr tuple))
    (setq position
          (instruction-index instruction))
    (when (debug-mode)
      (format t "~%Method-Body~%~A" (jana.java.jimple.conversion:jimple-statement (body method))))
    (cond ((typep (assignment-target instruction) 'jimple-reference-value-field)
           (setq reference-value (assignment-target instruction))
           (setq is-write t))
          ((typep (assignment-source instruction) 'jimple-reference-value-field)
           (setq reference-value (assignment-source instruction))
           (setq is-write nil)))
    (unless (= (mismatch (field-name reference-value) +this-field-prefix+)
               +this-field-prefix-length+)
      ;; public static synchronized void
      ;;  logFieldWrite(Object fieldOwnerInstance, String fieldOwnerClass, String aFieldName, String methodSignature)
      (setq bound-values
            (list
             :value (if (typep reference-value 'jimple-reference-value-instance-variable)
                        (local-variable-instance-reference reference-value)
                        (java-null-value))
             :constant (qualified-name (field-owner reference-value))
             :constant (field-name reference-value)
             :constant (full-java-method-signature method)))
      (if is-write
          (setq method-reference
                (make-log-field-write-reference))
          (setq method-reference
                (make-log-field-read-reference)))
      (setq
       invocation-instruction
       (make-static-invocation-instruction method-reference
                                           *monitor-aspect-class-name*
                                           bound-values))
      (if (equal (qualified-name method) "<init>")
          (setq constructor-call-position
           (locate-constructor-call (body method) :max-position position))
         (setq constructor-call-position
            -1))
      (if (<= position constructor-call-position)
            (insert-instructions-after
             (body method)
             (list invocation-instruction)
             (find-instruction-index (body method)  constructor-call-position :starting-at constructor-call-position))
      (insert-instructions-before
       (body method)
       (list invocation-instruction)
       (find-instruction-index (body method) position :starting-at position)))
      (when (debug-mode)
        (format t "~%Transformed Method-Body~%~A" (jana.java.jimple.conversion:jimple-statement (body method)))))))

(defun insert-log-ccsec-enter-call (tuple)
  (let ((method)
        (instruction)
        (instructions ())
        (position 0)
        (cast-instruction)
        (closure-context)
        (local-variable)
        (invocation-instruction))
    (setq method
          (car tuple))
    (setq instruction
          (cdr tuple))
    (setq position
          (instruction-index instruction))
    (setq closure-context
          (jimple-closure-context (body method)))
    (when (debug-mode)
      (format t "~%Method-Body~%~A"
              (jana.java.jimple.conversion:jimple-statement (body method))))
    (when (typep instruction 'jimple-ccsec-enter-instruction)
      (setq local-variable
            (gen-local-variable closure-context
                                (java-type "java.lang.Object")))
      (add-local-variables
       (dynamic-closure-context closure-context)
       (body method))
      (setq
       cast-instruction
       (jimple-imaginary-instruction-assignment
        (variable-reference local-variable)
        (jimple-cast (java-value instruction)
                     (java-type "java.lang.Object"))
        -1))
      ;; public static synchronized void lock(Object lockInstance)
      (setq
       invocation-instruction
       (make-static-invocation-instruction (make-lock-reference)
                                           *monitor-aspect-class-name*
                                           `(:variable ,local-variable)))
      (setq instructions
            (list cast-instruction invocation-instruction))
      (insert-instructions-before
       (body method)
       instructions
       (find-instruction-index (body method) position :starting-at position))
      (when (debug-mode)
        (format t "~%Transformed Method-Body~%~A" (jana.java.jimple.conversion:jimple-statement (body method)))))))

(defun insert-log-ccsec-exit-call (tuple)
  (let ((method)
        (instruction)
        (position 0)
        (invocation-instruction))
    (setq method
          (car tuple))
    (setq instruction
          (cdr tuple))
    (setq position
          (instruction-index instruction))
    (when (debug-mode)
      (format t "~%Method-Body~%~A"
              (jana.java.jimple.conversion:jimple-statement (body method))))
    (when (typep instruction 'jimple-ccsec-exit-instruction)
      ;; public static synchronized void unlock()
      (setq
       invocation-instruction
       (make-static-invocation-instruction (make-unlock-reference)
                                           *monitor-aspect-class-name*
                                           '()))
      (insert-instructions-after
       (body method)
       (list invocation-instruction)
       (find-instruction-index (body method) position :starting-at position))
      (when (debug-mode)
        (format t "~%Transformed Method-Body~%~A" (jana.java.jimple.conversion:jimple-statement (body method)))))))

;;; chr: TODO - need a better name -- this is the public interface! 
(defmethod add-synchronization-analysis ((project-name string) (repository-directory-name string) &optional (runtime-monitor "") (no-static-analyses nil))
  (if no-static-analyses
      (setq *use-static-analysis* nil)
      (setq *use-static-analysis* t))
  (when (> (length runtime-monitor) 0)
    (setq *monitor-aspect-class-name* runtime-monitor))
  (let ((start 0)
          (end 0)
        (project (load-java-project project-name repository-directory-name)))
    (setq start (get-internal-real-time))
    (load-java-classes project)
    (weave-eraser-aspect project)
    (save-java-project project)
    (setq end (get-internal-real-time))
    (jana.base:print-time-elapsed start end)))


;;; ESCAPE ANALYSIS

(defstruct method-arguments-pair
  (method-call)
  (arguments '() :type list))

;; might be interesting to keep track of the position in the method where the aliasing occured
(defclass intra-alias-summary ()
  ((closure
    :accessor closure
    :type java-closure
    :documentation "The closure for which the summary was produced.")
   (reference-values
    :accessor reference-values
    :type hash-set
    :documentation "Field-reference values and local-variable reference value
to which the aliased value was assigned.")
   (method-calls
    :accessor method-calls
    :type list
    :documentation "Methods that may be called with the aliased value as argument.")
   (return-instruction
    :accessor return-instruction
    :documentation "The return-instruction by which the aliased value
might be returned.")
   (array-assignment-instructions
    :accessor array-assignment-instructions
    :documentation "The array-assignment instructions to which
the aliased value was assigned."))
  (:documentation "The summary of aliases to which a value was assigned inside of a method."))
    
(defclass alias-set ()
  ((field-references 
    :accessor field-references
    :initform (make-hash-table :test #'equal))
   (local-variables 
    :accessor local-variables
    :initform (make-hash-table :test #'equal))
   (aliases 
    :accessor aliases
    :initform (util.collections:make-hash-set :test #'equal))))

(defun num-aliases (self)
  (declare #.*standard-optimize-settings*)
  (util.collections:hashset-count (slot-value self 'aliases)))

(defun add-alias (self value)
  "Adds the alias value to the alias-set self"
  (declare #.*standard-optimize-settings*)
  (cond 
    ((typep value 'jimple-reference-value-local)
     (setf 
      (gethash (unqualified-name value) (slot-value self 'local-variables))
      value))
    ((typep value 'jimple-reference-value-field)
     (setf 
      (gethash (cons (field-name value) (field-owner value)) 
               (slot-value self 'field-references))
      value))
    ((typep value 'jimple-reference-value-array)
     (setf
      (gethash 
       (unqualified-name (local-variable-instance-reference value))
       (slot-value self 'local-variables))
      (local-variable-instance-reference value))))
  (add-element value (slot-value self 'aliases)))

(defun alias-p (self value)
  "TESTS if the jimple-value VALUE is an alias stored in the 
alias-set object SELF."
  (declare #.*standard-optimize-settings*)
  (cond 
    ((contains-value-p value (slot-value self 'aliases))
     ;; it's really the same instance
     t)
    ((typep value 'jimple-reference-value-field)
     ;; field -- check name and owner type
      (gethash (cons (field-name value) (field-owner value)) 
               (slot-value self 'field-references)))
    ((typep value 'jimple-reference-value-array)
     ;; array -- check if the local variable used to reference it is the same 
      (gethash 
       (unqualified-name (local-variable-instance-reference value))
       (slot-value self 'local-variables)))
    ((typep value 'jimple-reference-value-local)
     ;; variable -- check name
     (gethash (unqualified-name value) (slot-value self 'local-variables)))
    (t
     nil)))

;; use the instance context to do a this-alias-analysis!
(defmethod alias-analysis ((closure jimple-closure) (value java-value))
  (declare #.*standard-optimize-settings*)
  (let ((iterations 0)
        (aliases-found -1)
        (alias-set (make-instance 'alias-set))
        (src)
        (dst))
    (declare (type fixnum iterations aliases-found))
    (loop 
       :while (not (= aliases-found (num-aliases alias-set)))
       :do
         (setq aliases-found
               (num-aliases alias-set))
         (incf iterations)
         (loop
            :for instruction :in (instructions closure)
            :when (typep instruction 'jimple-abstract-imaginary-assignment-instruction) ; only assignments can transfer object ownership
            :do ;;(format t ".")
                  (setq src (assignment-source instruction))
                  (setq dst (assignment-target instruction))
                  (cond 
                    ((reference-values-equal src value) ; chr: warning - only works for this references!
                     ;; easiest case -- THIS has been found on the RHS
                     (when (debug-mode)
                       (format t "~% [alias-analysis] 1 - Found VALUE in assignment source!"))
                     (add-alias alias-set value)
                     (add-alias alias-set dst))
                    ((alias-p alias-set src)
                     (format t "~% [alias-analysis] 2 - Found assignment source in alias set!")
                     (add-alias alias-set dst)))))
    (format t "~%Alias analysis terminated after ~A iterations. ~A aliases found" iterations aliases-found)
    (aliases alias-set)))


(defun aliased-method-arguments (method-invocation-instruction aliases)
  (declare #.*standard-optimize-settings*
           (type hash-set aliases))
  (loop :for argument :in (method-arguments method-invocation-instruction)
        :for argument-position :of-type fixnum :from 0
        :when (contains-value-p argument aliases)
        :collect argument-position))

(defmethod methods-receiving-value ((closure jimple-closure)
                                    (aliases hash-set))
  (declare #.*standard-optimize-settings*)
  (let ((argument-positions '())
        (method-argument-pairs '()))
    (declare (type list argument-positions method-argument-pairs))
    (dolist (instruction (instructions closure))
      (cond ((and (typep instruction
                         'jimple-abstract-imaginary-assignment-instruction)
                  (typep (assignment-source instruction)
                         'jimple-method-invocation-instruction))
             (setq argument-positions
                   (aliased-method-arguments
                    (assignment-source instruction)
                    aliases))
             (when argument-positions
               (push
                (make-method-arguments-pair
                 :method-call (assignment-source instruction)
                 :arguments argument-positions)
                method-argument-pairs)))
            ((typep instruction
                    'jimple-method-invocation-instruction)
             (setq argument-positions
                   (aliased-method-arguments
                    instruction
                    aliases))
             (when argument-positions
               (push
                (make-method-arguments-pair
                 :method-call instruction
                 :arguments argument-positions)
                method-argument-pairs)))))
    method-argument-pairs))
                 
;;; ESCAPE ANALYSIS

(defmethod escape-analysis ((closure jimple-closure)
                            (value java-value)
                            (this-class java-classifier-declaration)
                            (project java-project)
                            &optional (aliases (alias-analysis closure value)))
  (declare #.*standard-optimize-settings*
           (type hash-set aliases))
  (let ((method-argument-pairs '())
        (has-escaped nil)
        (method-call)
        (called-method))
    (declare (type list method-argument-pairs)
             (type boolean has-escaped))             
    (setq method-argument-pairs
          (methods-receiving-value closure aliases))
    (loop :for alias :being :the hash-keys :in (util.collections:hashset-table aliases)
          :do (when (typep alias 'jimple-reference-value-class-variable)
                (setq has-escaped t)
                (format  t "~%Escape! Assignment of value to class variable.")
                (return)))
    (unless has-escaped
      (dolist (method-argument-pair method-argument-pairs)
        (setq method-call
              (method-arguments-pair-method-call method-argument-pair))
        (cond ((equal
                (qualified-name (receiver-class method-call))
                (qualified-name this-class))
               (setq called-method
                     (find-java-method this-class
                                       (method-name (method-reference method-call))
                                       project))
               (cond ((has-modifier-p called-method 'native)
                      (setq has-escaped t))
                     ((eql (body called-method) closure)
                      (format t "~%Recursive Method Call: ~A.~A" (qualified-name (owner-class called-method)) (qualified-name called-method))
                      (format t "~%Call: ~A ~%Receiver: ~A" (jana.java.jimple.conversion:jimple-statement method-call) (qualified-name (receiver-class method-call)))
                      (setq has-escaped nil))
                     (t
                      (setq has-escaped
                            (escape-analysis (body called-method) value this-class project))))
               (when has-escaped
                 (return)))
              (t          
               (setq has-escaped t)
               (format  t "~%Escape! Call of method ~A with value as argument."
                        (jana.java.jimple.conversion:jimple-statement method-call))
               (return)))))
    (unless (or has-escaped
                (typep (last (instructions closure))
                       'jimple-return-void-instruction))
      (loop :for instruction :in (instructions closure)
            :when (and (typep instruction 'jimple-return-instruction)
                       (contains-value-p (return-value instruction)
                                         aliases))
            :do (setq has-escaped t)
                (format  t "~%Escape! Return of variable.")
                (return)))
    has-escaped))

(defmethod find-super-method-call ((self java-method-implementation) aliases class)
  "RETURNS the jimple-invoke-special-instruction that corresponds to the call of the
method SELF in the direct-superclass of the class CLASS,
and the name of the super-class.
The receiver of a super method-call is this, which is stored in one of the aliases
of the hash-set ALIASES."
  (declare #.*standard-optimize-settings*)
  (let ((super-method-call nil)
        (invoke-special-instruction nil)
        (super-class-name "")
        (current-method-name ""))
    (declare (type string super-class-name))
    (when (super-types class)
      (setq super-class-name
            (qualified-name (direct-super-type class)))
      (when (debug-mode)
        (format  t "~%Find super-method call: ~A for method ~A" super-class-name (unqualified-name self)))
      (dolist (instruction (instructions (body self)))
        (cond ((and (typep instruction
                           'jimple-abstract-imaginary-assignment-instruction)
                    (typep (assignment-source instruction)
                           'jimple-invoke-special-instruction))
               (setq invoke-special-instruction
                     (assignment-source instruction)))
              ((typep instruction
                      'jimple-invoke-special-instruction)
                (setq invoke-special-instruction
                      instruction))
              (t
               (setq invoke-special-instruction nil)))
        (when invoke-special-instruction
          (when (contains-value-p (local-variable invoke-special-instruction)
                                  aliases) ; call with receiver $this
            (setq current-method-name
                  (method-name (method-reference invoke-special-instruction)))
            (when (and (equal current-method-name (unqualified-name self))
                       (equal super-class-name
                              (qualified-name (receiver-class invoke-special-instruction))))
              (setq super-method-call
                    invoke-special-instruction)
              (return))))))
    (values super-method-call super-class-name)))

(defun super-type-list-as-string (class)
  "Converts the list of super-types of the java-class CLASS
into a string of qualified super-type names."
  (with-output-to-string (out-stream)
    (format out-stream "(")
    (loop :for super-type :in (super-types class)
          :do (format out-stream " ~A" (qualified-name super-type)))
    (format out-stream " )")))

(defmethod this-escape-analysis ((self java-method-implementation)
                                 (this-class java-classifier-declaration)
                                 (project java-project)
                                 (memoization-table hash-table))
  (declare #.*standard-optimize-settings*
           (type hash-table memoization-table))
  (let ((value (jimple-reference-value-this
                (make-java-object-reference-type (signature this-class))))
        (super-method-call)
        (super-method)
        (this-has-escaped nil)
        (super-class-is-analyzed nil)
        (this-aliases)
        (super-class)
        (super-class-name ""))
    (declare (type boolean this-has-escaped super-class-is-analyzed)
             (type string super-class-name))
    (when (verbose-mode)
      (format t "~%Escape Analysis in method ~A"
              (jana.java.jimple.conversion:jimple-statement self)))
    (setq this-aliases
           (alias-analysis (body self) value))
    (setq this-has-escaped
          (escape-analysis (body self) value this-class project this-aliases))
    (unless this-has-escaped
      (when (debug-mode)
        (format t "~%Searching super method call "))
      (multiple-value-setq (super-method-call super-class-name)
        (find-super-method-call self this-aliases this-class))
      (when super-method-call
        (multiple-value-setq (this-has-escaped super-class-is-analyzed)
          (gethash super-class-name memoization-table))
        ;(when super-class-is-analyzed
        ;  (format t "~%Using memoized escape-information!"))
        (unless super-class-is-analyzed
          ;; TODO: factor out the following code into a separate function
          (when (debug-mode)
            (format t "~%Analyzing Super Method-Call ~A"
                    (jana.java.jimple.conversion:jimple-statement super-method-call)))
          (setq super-class
                (java-class project super-class-name))
          (when (debug-mode)
            (format t "~%Class: ~A" (qualified-name this-class))
            (format t "~%Super Class: ~A" super-class-name))
          (setq super-method
                (find-java-method super-class
                                  (method-name (method-reference super-method-call))
                                  project))
          ;(format t "~%Escape Analysis in super-method ~A"
          ;(jana.java.jimple.conversion:jimple-statement super-method))
          ;; analyze all superclasses
          (when (java-method-equal self super-method)
            (error "Method and super-method are the same! ~A ~% ~A"
                     (jana.java.jimple.conversion:jimple-statement self)
                     (jana.java.jimple.conversion:jimple-statement super-method)))
          (if (java-classifier-equal this-class super-class)
              (error "Class and superclass are the same! ~A extends ~A"
                     (qualified-name this-class)
                     (super-type-list-as-string this-class))
              (progn (setq this-has-escaped
                           (this-escape-analysis super-method super-class project memoization-table))
                     (setf (gethash super-class-name memoization-table)
                           this-has-escaped))))))
    this-has-escaped))
  
