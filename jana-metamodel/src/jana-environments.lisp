(defclass environment ()
 (:DOCUMENTATION "An environment binds signatures <<as in jana-signature>> to language elements, and is used to look-up the language elements used in contexts 
different from the ones where they were defined.") 

(defclass global-environment (environment)
 ((identifier-environment
   :DOCUMENTATION "A special environment that contains all identifiers used 
in any program element."))
 (:DOCUMENTATION "The global-environment contains identifiers and language elements of global scope. For example packages and classes in Java."))

(defclass identifier-environment (environment)
 ((global-identifiers
   :TYPE hashmap<qualified-name,java-signature>)
  (local-identifiers
   :TYPE hashmap<unqualified-name, jana-name>
   :DOCUMENTATION "To save space, all local-identifiers are shared by the
lcoal-environments where they are defined. Each local-identifier can be
associated with one or several local-environments. The local identifiers form
thus a tree of depth 2 with the global environment as root node, and the local
environments as leafs (global (sig-local (local-env local-env))) ."))
(:DOCUMENTATION "The identifier environment is part of the global environment,
and contains all qualified names of global scope, as well as unqualified names,
that may be shared by many language elements in different local scopes."))

(defclass local-environment (environment)
 (:DOCUMENTATION "This is the tricky part, here we need a datastructure like a
tree, that allows sharing (in the nodes) while still permitting for per-local-environment data (leafs). Unfortunately it is not yet forseeable how this 
choice will influence the computational complexity of the static analysis.
In case of doubts it is maybe better to choose the least elaborate datastructure, to keep look-ups cheap."))

