;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: jana.mx.java.jimple; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        jimple-contexts.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Sun Jun 28 11:44:06 2009 (z)
;;; 
;;; Last-Updated: Mi Jan  6 22:27:58 2010 (+0100)
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

;;; Dynamic Contexts are initialized before the closure is transformed !

(defgeneric initialize-local-variables (self &key)
  (:DOCUMENTATION "Used to initialize the local variables that allow to access a context at run-time.
This method is called by the constructor to initialize newly created instances."))

;;; --- Closure Context ---

;;; it is possible to access all other contexts from the jimple closure context
;;; chr: there seems to be a correspondence between biggest context and smallest scope!
(defclass jimple-closure-context (jana-meta-level-context)
  ((classifier-context
    :ACCESSOR classifier-context
    :TYPE java-classifier-context
    :DOCUMENTATION "The java-classifier context,
which allows to refer to the class of the closure.")
   (instance-context
    :ACCESSOR instance-context
    :TYPE java-instance-context
    :DOCUMENTATION "The java-instance context,
which allows to refer to the current instance.")
   (method-invocation-contexts
    :READER method-invocation-contexts
    :INITFORM (util.collections:make-hash-set)
    :TYPE util.collections:hash-set
    :DOCUMENTATION "A list of method-invocation-contexts, which allow to refer
to the return value, and argument values of methods called from within the closure.")
   (static-closure-context
    :READER static-closure-context
    :INITARG :static-part
    :TYPE jimple-static-closure-context
    :DOCUMENTATION "Information about closures that is only available
at compile time (e.g. signature, etc.) when reflection is not used")
   (dynamic-closure-context
    :ACCESSOR dynamic-closure-context
    :INITARG :dynamic-part
    :TYPE jimple-dynamic-closure-context
    :DOCUMENTATION "Information associated with the currently executed method-body,
which is available at run-time (e.g. the current-thread).")
   )
  (:DOCUMENTATION "The closure contexts associated in Java with methods.
The closure-context allows to refer to the instnace-context, the signature of the closure,
the methods eventually invoked by the closure, and the current-thread executing the closure."))

(defmethod jimple-closure-context ((self jimple-closure))
  "Constructor.
RETURNS the jimple-closure-context consisting of
the active contexts of the jimple-closure SELF,
or a new empty jimple-closure-context instance if no contexts were active."
  (unless (slot-boundp self 'closure-context)
    (setf (closure-context self) (make-jimple-closure-context self)))
  (closure-context self))

;(defmacro bridge (accessor-name base-accessor-name class-name)
;  `(defmethod ,accessor-name ((self ,class-name))
;    (,accessor-name (,base-accessor-name self))))
;
;(bridge classifier-context jimple-closure-context jimple-closure)
;(bridge instance-context jimple-closure-context jimple-closure)
;(bridge method-invocation-contexts jimple-closure-context jimple-closure)
;(bridge static-closure-context jimple-closure-context jimple-closure)
;(bridge dynamic-closure-context jimple-closure-context jimple-closure)

(defmethod classifier-context ((self jimple-closure))
  (classifier-context (jimple-closure-context self)))

(defmethod instance-context ((self jimple-closure))
  (instance-context (jimple-closure-context self)))

(defmethod method-invocation-contexts ((self jimple-closure))
  (method-invocation-contexts (jimple-closure-context self)))

(defmethod static-closure-context ((self jimple-closure))
  (static-closure-context (jimple-closure-context self)))

(defmethod dynamic-closure-context ((self jimple-closure))
  (dynamic-closure-context (jimple-closure-context self)))


(defun make-jimple-closure-context (closure)
  "Constructor"
  (declare (type jimple-closure closure))
  (make-instance 'jimple-closure-context
                 :static-part (jimple-static-closure-context closure)
                 :dynamic-part (jimple-dynamic-closure-context closure)))


(defclass jimple-static-closure-context (jana-meta-level-context jana-compile-time-context)
  ((closure
    :READER closure
    :INITARG :closure
    :TYPE jimple-closure
    :DOCUMENTATION "A jimple-closure"))
  (:DOCUMENTATION "The static context associated with a closure in the Java language."))

(defun jimple-static-closure-context (closure)
  "Constructor"
  (declare (type jimple-closure closure))
  (make-instance 'jimple-static-closure-context
                 :closure closure))

(defclass jimple-dynamic-closure-context (jana-meta-level-context jana-run-time-context)
  ((local-variables
    :ACCESSOR local-variables
    :INITFORM '()
    :TYPE list
    :DOCUMENTATION "Fresh local variables generated for use by transformations.")
   (current-thread-context-variable
   :ACCESSOR current-thread-context-variable
    :DOCUMENTATION "A local variable that is used to reference the current Thread.
The current-thread can always be determined by the method Thread.currentThread,
because the advised method is executed in the context of the caller-Thread.")
   (method-argument-value-variables
    :ACCESSOR method-argument-value-variables
    :INITFORM '()
    :DOCUMENTATION "A list of local variables that can be used to refer to the arguments of a closure."))
  (:DOCUMENTATION "The context available while executing a method at run-time."))

(defun jimple-dynamic-closure-context (closure)
  "Constructor"
  (declare (type jimple-closure closure))
  (initialize-local-variables
   (make-instance 'jimple-dynamic-closure-context)
   :closure closure))

(defmethod initialize-local-variables ((self jimple-dynamic-closure-context) &key closure)
  "Initializes the local variables of a jimple-dynamic-closure-context instance SELF.
RETURNS the initialized instance."
  (declare #.*standard-optimize-settings*)
  (let ((jana-type (java-object-reference-type (java-signature +thread-class-signature+)))
        (argument-value-variables '()))
    (declare (type list argument-value-variables))
    ;; initialize current-thread context-variable
    (setf (current-thread-context-variable self)
          (jimple-variable +current-thread-context-variable-name+ jana-type))
    ;; initialize arguments
    (loop :for argument-type :in (argument-types closure)
          :for argument-count :of-type fixnum :from 0
          :do (push
               (jimple-variable (concatenate 'string
                                             +argument-context-variable-prefix+
                                             "_"
                                             (princ-to-string argument-count))
                                argument-type)
               argument-value-variables))
    (when argument-value-variables
      (setf (method-argument-value-variables self)
            (nreverse argument-value-variables)))
    self))
  
#.(declaim (inline gen-local-variable))
(defmethod gen-local-variable ((self jimple-dynamic-closure-context) (type jana-type))
  "Generates a fresh local variable with the jana-type TYPE,
 for use by transformations in the java-dynamic-closure-context SELF.
RETURNS the newly created jimple-variable."
  (declare #.*standard-optimize-settings*)
  (let ((var (jimple-variable
              (string-downcase (symbol-name (gensym "$dcc_lv_")))
              type)))
    (setf (local-variables self)
          (nconc (local-variables self) (list var)))
    var))

(defmethod gen-local-variable ((self jimple-closure-context) (type jana-type))
  "Generates a fresh local variable with the jana-type TYPE,
 for use by transformations in the dynamic-closure-context of the jimple-closure-context SELF."
  (declare #.*standard-optimize-settings*)
  (gen-local-variable (dynamic-closure-context self) type))

;;;; --- Method-Invocation Context ---

(defclass jimple-method-invocation-context (jana-meta-level-context jana-compile-time-context)
  ((method-reference
    :ACCESSOR method-reference
    :INITARG :method-reference
    :TYPE jimple-reference-value-method
    :DOCUMENTATION "A reference to a Java method.")
   (method-return-value-variable
    :ACCESSOR method-return-value-variable
    :DOCUMENTATION "A local variable that is used to reference the value returned by a method.")
   (context-id
    :INITFORM (string-downcase (symbol-name (gensym)))
    :TYPE string
    :DOCUMENTATION "The private unique identifier used to generate unique names for variables associated with this context."))
  (:DOCUMENTATION "Abstract class whose methods allow to refer to the values returned by a method-call."))

(defmethod initialize-local-variables ((self jimple-method-invocation-context) &key)
  "Initializes the local variables of a jimple-method-invocation-context instance SELF.
RETURNS the initialized instance."
  (declare #.*standard-optimize-settings*)
  (let ((return-value-type (jana-type (method-reference self)))
        (context-id (slot-value self 'context-id)))
    (declare (type java-type return-value-type)
             (type string context-id))
    ;; initialize return-value
    (if (typep return-value-type 'java-void-type)
        ;; set to nil if no value is returned by the called-method
        (setf (method-return-value-variable self)
              nil)
        ;; otherwise create a variable with type return-type
        (setf (method-return-value-variable self)
              (jimple-variable (concatenate 'string
                                            +return-value-context-variable-prefix+
                                            context-id)
                               return-value-type)))
    ;; return initialized instance
    self))


(defclass java-referenced-method-invocation-context (jimple-method-invocation-context)
  ()
  (:DOCUMENTATION "Invocation context for a referenced method, whose declaration is not necessarily known statically.
An example are methods of classes that are linked dynamically, i.e. which reside in libraries that are linked at compile time."))

(defun java-referenced-method-invocation-context (reference-value-method)
  "Constructor"
  (declare #.*standard-optimize-settings*
           (type jimple-reference-value-method reference-value-method))
  (initialize-local-variables
   (make-instance 'jimple-method-invocation-context
                  :method-reference reference-value-method)))


(defclass java-declared-method-invocation-context (jimple-method-invocation-context java-method-context)
  ((method-exception-variables
    :ACCESSOR method-exception-variables
    :INITFORM '()
    :TYPE list
    :DOCUMENTATION "A list of local variables that can be used to refer to the exceptions eventually thrown by a method."))
  (:DOCUMENTATION "Invocation context for a method whose declaration is known.
An example are methods of classes that are part of the project."))

(defun java-declared-method-invocation-context (method-declaration)
  "Constructor.
METHOD-DECLARATION -- the called method."
  (initialize-local-variables
   (make-instance 'java-declared-method-invocation-context
                  :method-declaration method-declaration
                  :method-reference (jimple-reference-value-from-declaration method-declaration))))

(defmethod initialize-local-variables ((self java-declared-method-invocation-context) &key)
  "Initializes the local variables of a jimple-method-invocation-context instance SELF.
RETURNS the initialized instance."
  (declare #.*standard-optimize-settings*)
  (let ((exception-variables '())
        (context-id (slot-value self 'context-id)))
    (declare (type list exception-variables)
             (type string context-id))
    ;; initialize thrown exceptions
    (loop :for exception-type :in (thrown-exceptions (method-declaration self))
          :for exception-count :of-type fixnum :from 0
          :do (push
               (jimple-variable (concatenate 'string
                                             +exception-context-variable-prefix+
                                             context-id
                                             "-"
                                             (princ-to-string exception-count))
                                exception-type)
               exception-variables))
    (when exception-variables
      (setf (method-exception-variables self)
            (nreverse exception-variables)))
    (call-next-method self)))

;;; --- Instruction-Level Contexts chr: TODO ---

(defclass jimple-instruction-level-context (jana-meta-level-context jana-run-time-context)
  ((instruction-index
    :ACCESSOR instruction-index
    :TYPE fixnum
    :DOCUMENTATION "The position in the instruction list."))
  ;; chr: allows to reflect about instructions at compile-time
  (:DOCUMENTATION "The instruction-level-context provides informations about an instruction."))


(defclass jimple-field-assignment-level-context (jimple-instruction-level-context)
  ((field
    :ACCESSOR field
    :TYPE jimple-reference-value-field
    :DOCUMENTATION "The field that is assigned."))
  (:DOCUMENTATION "Provides information about field assignments."))

#.(declaim (inline field-name))
(defmethod field-name ((self jimple-field-assignment-level-context))
  "RETURNS the field-name string."
  (field-name (field self)))

(defclass jimple-field-write-context (jimple-field-assignment-level-context)
  ((assigned-value
    :ACCESSOR assigned-value
    :TYPE jimple-value
    :DOCUMENTATION "The value assigned to the field."))
   (:DOCUMENTATION "The context where an instruction writes a field."))

(defun jimple-field-write-context (imaginary-assignment-instruction position)
  "Constructor"
  (declare (type jimple-abstract-imaginary-assignment-instruction imaginary-assignment-instruction)
           (type fixnum position))
  (let ((instance (make-instance 'jimple-field-write-context ))
        (assignment-target (assignment-target imaginary-assignment-instruction)))
    (setf (instruction-index instance)
          position)
    (if (typep assignment-target 'jimple-reference-value-field)
        (setf (field instance)
              assignment-target)
        (error "The instruction ~A does not assign to a field!" imaginary-assignment-instruction))
    (setf (assigned-value instance)
          (assignment-source imaginary-assignment-instruction))
    instance))

(defclass jimple-field-read-context (jimple-field-assignment-level-context)
  ()
  (:DOCUMENTATION "The context where an instruction reads a field."))

(defun jimple-field-read-context (imaginary-assignment-instruction position)
  "Constructor"
  (declare (type jimple-abstract-imaginary-assignment-instruction imaginary-assignment-instruction)
           (type fixnum position))
  (let ((instance (make-instance 'jimple-field-read-context ))
        (assignment-source (assignment-source imaginary-assignment-instruction)))
    (setf (instruction-index imaginary-assignment-instruction)
          position)
    (if (typep assignment-source 'jimple-reference-value-field)
        (setf (field instance)
              assignment-source)
        (error "The instruction ~A does not read a field!" imaginary-assignment-instruction))
    instance))

;;; --- METHODS (TRANSFORMATION) ---

(defun make-static-call-assignment (method-reference variable-reference arguments)
  "Creates an assignment-instruction, whose source is a
static method-call of the method METHOD-REFERENCE.
The return value of this method is assigned by the instruction
to the variable VARIABLE-REFERENCE."
  (declare #.*standard-optimize-settings*
           (type jimple-reference-value-method method-reference)
           (type jimple-reference-value-local variable-reference))
  (jimple-imaginary-instruction-assignment
   variable-reference
   (jimple-invoke-static-instruction
    (jana-type method-reference)
    method-reference
    arguments)
   -1))

(defmethod find-context-variable-declaration ((closure jimple-closure) variable-name)
  "Checks if the jimple-closure CLOSURE already declares
a context-variable with the name VARIABLE-NAME.
RETURNS the local-variable-declaration and position in the list
of variable-declarations, or NIL, if no match is found."
  (declare #.*standard-optimize-settings*
           (type string variable-name))
  (let ((variable-declarations (local-variables closure)))
    (declare (type list variable-declarations))
    (loop
        :for local-variable-declaration :in variable-declarations
        :for position :of-type fixnum :from 0 :by 1
        :do
         (when (equal variable-name
                      (unqualified-name local-variable-declaration))
           (return (values local-variable-declaration position)))
        :finally
         (return (values NIL -1)))))

#.(declaim (inline find-last-initialization-statement))
(defun find-last-initialization-statement (closure)
  "RETURNS the position of the last initialization statement."
  (declare #.*standard-optimize-settings*
           (type jimple-closure closure))
  (loop
      :for instruction :in (instructions closure)
      :for position :of-type fixnum :from 0
      :do
        (unless (or (typep instruction
                           'jimple-imaginary-instruction-variable-initialization)
                    (typep instruction
                           'jimple-invoke-special-instruction))
          (return (- position 1)))
      :finally (return position)))

(defmethod find-this-assignment ((closure jimple-closure))
  "RETURNS the assignment instruction that assigns a this-reference
to a local variable, and the position in the instructions list
of the jimple-closure CLOSURE, or NIL, if no such instruction is found."
  (declare #.*standard-optimize-settings*)
  (let ((instructions (instructions closure)))
    (declare (type list instructions))
    (loop
        :for instruction :in instructions
        :for position :of-type fixnum :from 0 :by 1
        :do 
         (when (and (typep instruction
                           'jimple-imaginary-instruction-variable-initialization)
                    (typep (assignment-source instruction)
                           'jimple-reference-value-this))
           ; (format t "Found this assignment: ~%~A:~A" jimple-instruction position)
           (return (values instruction position)))
        :finally
         (return (values NIL -1)))))

;;; --- JAVA classifier context ---

#.(declaim (inline add-instance-context-to-closure))
(defmethod add-classifier-context-to-closure ((self java-classifier-context) (closure jimple-closure))
  "Private. Initializes the classifier-context slot of the jimple-closure-context
associated with the jimple-closure CLOSURE using the java-classifier-context object SELF."
  (declare #.*standard-optimize-settings*)
  (setf (classifier-context (jimple-closure-context closure))
        self))

(defmethod add-classifier-context ((self java-classifier-context) (closure jimple-closure))
  "Adds the java-classifier-context SELF to the jimple-closure CLOSURE."
  (declare #.*standard-optimize-settings*)
  (add-classifier-context-to-closure self closure))


;;; --- JAVA instance context ---

#.(declaim (inline add-instance-context-to-closure))
(defmethod add-instance-context-to-closure ((self java-instance-context) (closure jimple-closure))
  "Private. Initializes the instance-context slot of the jimple-closure-context
associated with the jimple-closure CLOSURE using the java-instance-context object SELF."
  (declare #.*standard-optimize-settings*)
  (setf (instance-context (jimple-closure-context closure))
        self))

#.(declaim (inline make-this-reference-value))
(defmethod make-this-reference-value ((self java-instance-context))
  "Creates the value necessary to refer to *this*."
  (declare #.*standard-optimize-settings*)
  (jimple-reference-value-this (jana-type self)))

(defmethod add-this-context ((self java-instance-context) (closure jimple-closure))
  "Adds an assignment-instruction to the closure jimple-closure,
that assigns the value of *this* to the this-context-variable."
  (declare #.*standard-optimize-settings*)
  (let ((this-cvar (this-context-variable self))
        (last-init-position 0)
        (this-init-instruction nil))
    (declare (type jimple-variable this-cvar)
             (type fixnum last-init-position))
    (unless (find-context-variable-declaration closure +this-context-variable-name+)
      (setq this-init-instruction
            (nth-value 0 (find-this-assignment closure)))
      (if this-init-instruction
          ;; case (a): splice-in a new assignment instruction
          (progn
            (setq last-init-position
                  (find-last-initialization-statement closure))
            ;(format t "~%Last initialization in: ~A" last-init-position)
            (setq this-init-instruction
                  (jimple-imaginary-instruction-assignment
                   (variable-reference this-cvar)
                   (assignment-target this-init-instruction)
                   -1))
            (insert-instructions-after
             closure
             (list this-init-instruction)
             last-init-position))
          ;; case (b): initialize the context-variable with the this reference
          (progn           
            (setq this-init-instruction
                  (jimple-imaginary-instruction-variable-initialization
                   (variable-reference this-cvar)
                   (make-this-reference-value self)
                   -1))
            (push this-init-instruction
                  (instructions closure))))
      (add-context-variable closure this-cvar)
      (add-instance-context-to-closure self closure))))


(defun make-get-class-method-reference ()
  "RETURNS a method-reference of type jimple-reference-value-method
for the method +GETCLASS-METHOD-NAME+ that is used in Java
to get the class object of an instance."
  (jimple-reference-value-method
   +getclass-method-name+
   (make-java-object-reference-type (java-signature +class-signature+))
   '()))

(defmethod add-this-class-context ((self java-instance-context) (closure jimple-closure))
  "Adds the variables defined by the java-instance-context SELF,
 and the corresponding initialization instructions to the jimple-closure CLOSURE."
  (declare #.*standard-optimize-settings*)
  (unless (find-context-variable-declaration closure +this-class-context-variable-name+)
    (add-this-context self closure)
    (let ((this-class-cvar (this-class-context-variable self))
          (assignment-instruction))
      ;; create assignment instruction
      (setq assignment-instruction
            (make-static-call-assignment (make-get-class-method-reference)                                    
                                         (variable-reference this-class-cvar)
                                         '()))
      ;; add instruction after last initialization
      (insert-instructions-after
       closure
       (list assignment-instruction)
       (find-last-initialization-statement closure))
      ;; add variable declaration
      (add-context-variable closure this-class-cvar)
      (add-instance-context-to-closure self closure))))

(defmethod add-java-instance-context ((self java-instance-context) (closure jimple-closure))
  "TRANSFORMS the method body of the jimple-closure CLOSURE and adds the instructions necessary
to assign the context information to the local-variables of the java-instance-context SELF.
This transformation can be applied only once."
  (unless (slot-boundp (jimple-closure-context closure) 'instance-context)
    (add-this-context self closure)
    (add-this-class-context self closure)))

;;; --- Jimple dynamic closure-context ---

#.(declaim (inline add-dynamic-closure-context-to-closure))
(defmethod add-dynamic-closure-context-to-closure ((self jimple-dynamic-closure-context) (closure jimple-closure))
  "Private. Initializes the dynamic-closure-context slot of the jimple-closure-context
associated with the jimple-closure CLOSURE using the jimple-dynamic-closure-context object SELF."
  (declare #.*standard-optimize-settings*)
  (setf (dynamic-closure-context (jimple-closure-context closure))
        self))

(defmethod add-argument-value-variables ((self jimple-dynamic-closure-context) (closure jimple-closure))
  "Adds local variables that are necessary to refer to the arguments of a jimple-closure CLOSURE
using the jimple-dynamic-closure-context SELF."
  (unless (or (= (length (method-argument-value-variables self)) 0)
              (find-context-variable-declaration closure (first (method-argument-value-variables self))))
    (let ((variable-reference)
          (instructions '())
          (argument-variable-references '()))
      ;; create assignment instructions
      (loop :for variable :in (method-argument-value-variables self)
            :for count :of-type fixnum :from 0
            :do (setq variable-reference
                      (jimple-reference-value-from-declaration variable))
                (push variable-reference
                      argument-variable-references)
                (push (jimple-imaginary-instruction-variable-initialization
                       variable-reference
                       (jimple-reference-value-argument count (jana-type variable))
                       -1)
                      instructions))
    ;; add assignment instructions after this-assignment
    (insert-instructions-after
     closure
     instructions
     (find-last-initialization-statement closure))
    ;; add variable declarations
    (loop :for variable :in (method-argument-value-variables self)
          :do (add-context-variable closure variable))
    ;; add context to closure      
    (add-dynamic-closure-context-to-closure self closure))))

(defun make-current-thread-method-reference ()
  "RETURNS a method-reference of type jimple-reference-value-method
for the method +CURRENT-THREAD-METHOD-NAME+
that is used in Java to get the thread object
for the currently executing thread."
  (jimple-reference-value-method
   +current-thread-method-name+
   (make-java-object-reference-type (java-signature +thread-class-signature+))
   '()))

(defmethod add-current-thread-initialization ((self jimple-dynamic-closure-context) (closure jimple-closure))
  "Thread.currentThread()"
  (declare #.*standard-optimize-settings*)
  ;; chr: the first condition is currently not necessary, as dynamic-part is initialized during construction
  ;;      may be needed for lazy initialization
  (unless (find-context-variable-declaration closure +current-thread-context-variable-name+)
    (let ((current-thread-cvar (current-thread-context-variable self))
          (assignment-instruction))
      ;; create assignment instruction
      (setq assignment-instruction
            (make-static-call-assignment (make-current-thread-method-reference)
                                         (variable-reference current-thread-cvar)
                                         '()))
      ;; add instruction after this-assignment
      (insert-instructions-after
       closure
       (list assignment-instruction)
       (find-last-initialization-statement closure))
      ;; add variable declaration
      (add-context-variable closure current-thread-cvar)
      ;; add context to closure      
      (add-dynamic-closure-context-to-closure self closure))))

(defmethod add-local-variables ((self jimple-dynamic-closure-context) (closure jimple-closure))
  "Adds the local variables to the jimple-closure CLOSURE that were created by
requesting new local variables from the jimple-dynamic-closure-context SELF
using the method gen-local-variable."
  (declare #.*standard-optimize-settings*)
  (when (debug-mode)
    (format t "~%Adding ~A local variables" (length (local-variables self))))
  (dolist (context-variable (local-variables self))
    ;; add variable declarations
    (unless (find-context-variable-declaration closure (qualified-name context-variable))
      (add-context-variable closure context-variable)))
  ;; add context to closure      
  (add-dynamic-closure-context-to-closure self closure))

(defmethod add-jimple-dynamic-closure-context ((self jimple-dynamic-closure-context) (closure jimple-closure))
 "TRANSFORMS the method body of the jimple-closure CLOSURE and adds the instructions necessary
to assign the context information to the local-variables of the jimple-closure-context SELF.
Adds all local-variables, and the context-variable for the current thread as well.
This transformation can be applied only once."
  (unless (slot-boundp (jimple-closure-context closure) 'dynamic-closure-context)
    ; (add-current-thread-initialization self closure)
    (add-argument-value-variables self closure)
    (add-local-variables self closure)))

;;; --- Java method-invocation context ---

#.(declaim (inline add-method-invocation-context-to-closure))
(defmethod add-method-invocation-context-to-closure ((self jimple-method-invocation-context) (closure jimple-closure))
  "Private. Adds a jimple-invocation-context SELF to the jimple-closure CLOSURE.
Add synchronization here if it is necessary to create multiple method-invocation-contexts concurrently.
This should NOT be necessary, as creating method-invocation-contexts is relatively cheap and
transformations of a jimple-closure have to be applied serially, for example, by synchronizing
on the closure or class-level."
  (declare #.*standard-optimize-settings*)
  (let ((closure-context (jimple-closure-context closure)))
    (util.collections:add-element (slot-value closure-context 'method-invocation-contexts)
                                  self)))

(defmethod add-return-value-variable ((self jimple-method-invocation-context) (closure jimple-closure))
  "Adds local variables necessary to refer to the return-value of a called method."
  (declare #.*standard-optimize-settings*)
  (unless (find-context-variable-declaration closure (qualified-name (method-return-value-variable self)))
    (add-context-variable closure (method-return-value-variable self))))

(defmethod add-exception-variables ((self java-declared-method-invocation-context) (closure jimple-closure))
  "Adds local variables necessary to refer to the exceptions that may be thrown by  a called method."
  (declare #.*standard-optimize-settings*)
  (unless (or (= (length (method-exception-variables self)) 0)
              (find-context-variable-declaration closure (qualified-name (first (method-exception-variables self)))))
    (dolist (variable (method-exception-variables self))
      (add-context-variable closure variable))))

;;; chr: probably belongs to jimple-cflow

(defmethod make-static-invocation-instruction ((self jimple-reference-value-method)
                                               (class-name string)
                                               (binding-declaration list))
  "Creates a static invocation instruction provided the class-name CLASS-NAME of the callee class,
the reference to the called method SELF, and a binding declaration BINDING-DECLARATION
consisting of string-values."
  (declare #.*standard-optimize-settings*)
  (let ((invocation-instruction))
    (setq invocation-instruction    
          (jimple-invoke-static-instruction
           (java-object-reference-type (java-signature class-name))
           self
           '()))
    (apply #'bind `(,invocation-instruction ,@binding-declaration))
    invocation-instruction))
  
(defmethod make-method-invocation-instruction ((self jimple-reference-value-method)
                                               local-variable
                                               (binding-declaration list))
  "Creates an invoke-virtual instruction provided a local variable LOCAL-VARIABLE that
references the callee-object, as well as the reference to the called method SELF,
and a binding declaration BINDING-DECLARATION consisting of string-values."
  (declare #.*standard-optimize-settings*)
  (let ((invocation-instruction))
    (setq invocation-instruction
          (jimple-invoke-virtual-instruction  local-variable
                                              (jana-type local-variable)
                                              self
                                              '()))
    (apply #'bind `(,invocation-instruction ,@binding-declaration))
    invocation-instruction))

(defmethod bind ((self jimple-method-invocation-instruction) &rest values)
"Beware! Currently no type-checking is applied,
it is only checked if the binding-specification binds all arguments.
The following keywords can be used to bind values:
 :constant - converts the value following the keyword to a java-constant-value
 :value - expects the following element to be a jimple-value that is passed as argument
 :variable - expects the following element to be a jimple-variable that is passed as argument
 :ignore - don't modify the binding of the argument -- throws an exception if no binding exists."
  (declare #.*standard-optimize-settings*
           (type list values))
  (let ((values-list values)
        (value)
        (bound-values '())
        (num-values-bound 0))
    (declare
     (type list values-list bound-values)
     (type fixnum num-values-bound))
    (when (debug-mode)
      (format t "~%Binding declaration: ~A" values-list))
    (loop
      :while values-list
      :do (setq value (first values-list))
          (cond ((eq value :constant)
                 (push (java-constant-value (second values-list))
                       bound-values)
                 (setq values-list (cdr values-list)))
                ((eq value :value)
                 (push (second values-list)
                       bound-values)
                 (setq values-list (cdr values-list)))
                ((eq value :variable)
                 (push (variable-reference (second values-list))
                       bound-values)
                 (setq values-list (cdr values-list)))
                ((eq value :ignore)
                 (push (nth num-values-bound (method-arguments self))
                       bound-values)
                 (setq values-list (cdr values-list)))
                (t
                 (error "Found invalid key ~A in binding specification ~A ~A." value values values-list)))
          (incf num-values-bound)
          (setq values-list
                (cdr values-list)))
    ;; sanity check
    (unless
        (= (length bound-values)
           (length (argument-types self)))
      (error "Wrong number of bound-values! Method has ~A arguments.~%Values: ~A"
             (length bound-values) (length (argument-types self))))
    ;; chr: here could come a type-check
    (setq bound-values
          (nreverse bound-values))
    (when (debug-mode)
      (format t "Rebinding ~A ~A" (method-arguments self) bound-values))
    (setf (method-arguments self)
          bound-values)))