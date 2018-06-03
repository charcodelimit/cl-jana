(asdf:defsystem :eager-future
  :serial t
  :components ((:file "package")
               (:file "future")
               (:file "threads")
               (:file "eager-future"))
  :depends-on (:bordeaux-threads))

(asdf:defsystem :eager-future.test
  :components ((:module :test
                        :serial t
                        :components ((:file "package")
                                     (:file "test"))))
  :depends-on (:eager-future :fiveam))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :eager-future))))
  (asdf:oos 'asdf:load-op :eager-future)
  (asdf:oos 'asdf:load-op :eager-future.test)
  (funcall (intern (string '#:run-tests) (string '#:eager-future.test))))
