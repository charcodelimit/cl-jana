(in-package #:eager-future)

(defclass thread-pool ()
  ((threads :accessor threads :initform ())
   (free-thread-counter :accessor free-thread-counter :initform 0)
   (soft-limit :accessor soft-limit :initform nil)
   (lock :reader lock :initform (make-lock "thread pool lock"))
   (leader-notifier :reader leader-notifier :initform (make-condition-variable))
   (tasks :accessor tasks :initform nil)))

(defvar *thread-pool* (make-instance 'thread-pool))

(define-symbol-macro %thread-pool-soft-limit (soft-limit *thread-pool*))

(defun new-worker-thread (thread-pool task)
  (push (make-thread
         (lambda ()
           (unwind-protect
                (loop
                   (when task (ignore-errors (funcall task)))
                   (with-lock-held ((lock thread-pool))
                     (if (and (soft-limit thread-pool)
                              (> (length (threads thread-pool))
                                 (soft-limit thread-pool)))
                         (return)
                         (incf (free-thread-counter thread-pool)))
                     (setf task
                           (or #1=(pop (tasks thread-pool))
                               (progn
                                 (condition-wait (leader-notifier thread-pool)
                                                 (lock thread-pool))
                                 #1#)))
                     (decf (free-thread-counter thread-pool))))
             (with-lock-held ((lock thread-pool))
               (setf (threads thread-pool)
                     (remove (current-thread) (threads thread-pool))))))
         :name "Eager Futures Thread Pool Worker")
        (threads thread-pool)))

(defmethod assign-task (task (thread-pool thread-pool))
  (with-lock-held ((lock thread-pool))
    (if (= (free-thread-counter thread-pool) (length (tasks thread-pool)))
        (new-worker-thread thread-pool task)
        (push task (tasks thread-pool)))
    (condition-notify (leader-notifier thread-pool))))
