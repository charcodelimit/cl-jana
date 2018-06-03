(in-package #:eager-future)

(defclass future ()
  ((lock :reader lock :initform (make-lock "future lock"))
   (done? :accessor done? :initform nil)
   (execution-error :reader execution-error :initform nil)
   (values-yielded :reader values-yielded :initform nil)
   (wait-list :accessor wait-list :initform ())))

(defun make-task (thunk future)
  (lambda ()
    (unwind-protect
         (handler-case (setf (slot-value future 'values-yielded) (multiple-value-list (funcall thunk)))
           (condition (e) (setf (slot-value future 'execution-error) e)))
      (with-lock-held ((lock future))
        (setf (done? future) t))
      (dolist (x (wait-list future))
        (with-lock-held ((car x)))
        (condition-notify (cdr x))))))

(defun ready-to-yield? (future) ;; note that this can be defined in terms of select
  (with-lock-held ((lock future))
    (done? future)))

(defun select (&rest futures)
  "Returns the first future that is ready to yield."
  (let ((notifier (make-condition-variable))
        (our-lock (make-lock)))
    (with-lock-held (our-lock) ;; ensure that futures can't yield unless they have our lock
      (dolist (f futures)
        (with-lock-held ((lock f))
          (when (done? f)
            (return-from select f))
          (push (cons our-lock notifier) (wait-list f))))
      (loop (dolist (f futures)
              (when (ready-to-yield? f)
                (return-from select f)))
         (condition-wait notifier our-lock)))))

(define-condition execution-error (error)
  ((cause :initarg :cause :reader execution-error-cause)))

(defmethod print-object ((e execution-error) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (format stream "Caused by:~A" (execution-error-cause e))))

(defun yield (future)
  "Block until future is ready to yield. If the future yielded an
error, it is re-raised as an EXECUTION-ERROR, otherwise the values
yielded by the future are returned."
  (or (ready-to-yield? future)
      (select future))
  (if (execution-error future)
      (error 'execution-error :cause (execution-error future))
      (values-list (values-yielded future))))
