(cl:defpackage #:eager-future
  (:use #:common-lisp #:bordeaux-threads)
  (:export #:pcall #:pexec #:plet #:future
           #:select #:ready-to-yield? #:yield
           #:execution-error #:execution-error-cause
           #:%thread-pool-soft-limit))
