Making Releases
===============

A stand-alone executable of Jana can be built by loading the 
file `make-release.lisp` or `make-test-release.lisp`.

Release Mode
------------

These scripts bind the global variable `cl-user:*release-mode*` to `T`.
This variable is used to distinguish *release-mode* from *development-mode*.

In release mode, the directory where all source-files reside, is bound
dynamically. 

This has the following effect: 
the current path will not be included as a compile-time constant 
into the final lisp-image.

Development Mode
----------------

In *development-mode* the variable `cl:*default-pathname-defaults*` 
is assigned to the directory where the source-files reside.

Therefore, it is possible to use a repository that resides 
in this directory, and relative pathnames to load individual 
components of Jana
