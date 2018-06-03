(in-package #:eager-future)

(defun pcall (thunk)
  (let ((future (make-instance 'future)))
    (assign-task (make-task thunk future) *thread-pool*)
    future))

(defmacro pexec (&body body)
  `(pcall (lambda () ,@body)))

(defmacro plet ((&rest bindings) &body body)
  (let ((syms (mapcar (lambda (x)
                        (gensym (string (car x))))
                      bindings)))
    `(let ,(loop for (nil exp) in bindings
                 for sym in syms
                 collect `(,sym (pexec ,exp)))
       (symbol-macrolet ,(loop for (var nil) in bindings
                               for sym in syms
                               collect `(,var (yield ,sym)))
         ,@body))))
