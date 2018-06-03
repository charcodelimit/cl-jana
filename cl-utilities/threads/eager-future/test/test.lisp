(in-package #:eager-future.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :eager-future))
(in-suite :eager-future)

(defun run-tests ()
  (run! :eager-future))

(test sanity
      (is (equal '(1 2 3) (yield (pexec (list 1 2 3)))))
      (let ((task (pexec (+ 4 2))))
        (sleep .01)
        (is (= 6 (yield task)))))

(test stress
  (flet ((compute ()
           (loop :for i :from 0 :below 100000
                 :sum (* i i))))
      (let ((tasks (loop repeat 100 collect (pcall #'compute)))
            (answer (compute)))
        (sleep .05)
        (is (every (lambda (tsk) (= (yield tsk) answer)) tasks)))))

(test multi-yield
      (let* ((task (pexec (sleep .1) :ok))
             (yielders (loop :for i :from 0 :below 10
                          :collect (pexec (yield task)))))
        (sleep .01)
        (is (every (lambda (tsk) (eq (yield tsk) :ok)) yielders))))

(test plet
      (plet ((x (list 1 2))
             (y (list 3 4)))
            (sleep .01)
            (is (equal '(1 2 3 4) (append x y)))))

(test delayed-signal
      (let ((task (pexec (error "Wrong!"))))
        (sleep .01)
        (signals error (yield task))))

(test select-random
      (is (member (yield (select (pexec (sleep (random 0.2)) 1)
                                     (pexec (sleep (random 0.2)) 2)
                                     (pexec (sleep (random 0.2)) 3))) '(1 2 3))))

(test select-always
      (is (= 2 (yield (select (pexec (sleep 0.05) 1) (pexec 2))))))

(test select-all-tasks-done
      (flet ((make-done-task (val)
               (let ((task (pexec val)))
                 (yield task)
                 task)))
        (is (= 1 (yield (apply #'select (mapcar #'make-done-task '(1 2 3))))))))

(test select-error
      (signals execution-error (yield (select (pexec (sleep 0.01) 1)
                                                  (pexec (error "Error"))))))
