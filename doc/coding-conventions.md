Used Coding Conventions
=======================

Constructors
------------

The constructors provided by the public interface have the same name as the class
of which they create an instance.
Constructors are prefixed with "make-" when memoization is used. 
The public interface provides then the interface to the memoizing constructor,
and the "make-" prefixed constructor provides the interface for creating fresh 
instances without memoization.

Packages
--------

Packages are structured such, that a top-level package exists, which provides the public
application-programmer interface. Furthermore, a base-package is used sometimes to 
avoid circular dependencies among packages. The base-package provides then the 
functionality that other packages may rely-on.

