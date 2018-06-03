CSS: style.css
HTML use syntax: true
LaTeX use listings: true

{:cl:   lang=lisp code_background_color='#ffefef'}
{:syntax:  code_background_color='#efefff'}

CLOS-PATH
=========

Design
------

The CLOS-PATH system consists of the following parts:
- reader macros to declare queries
- a query lexer
- a query parser
- the query compiler
- query primitives

# The Reader Macros #

The reader macros are the user-interface of the CLOS-PATH system.
Two type of queries are supported: existential-queries and element-queries 
(in the future support may be added for traversal queries).
Existential-queries return T or NIL depending on whether elements
would be selected by a query expression. 
(There is some room for optimization here, as the query can be terminated,
 as soon as one element has been found.)
Element-queries return all elements that are selected by a query expression.

# Terminology #

## elements ##

Example:

        #$object/attr1
{:cl:}

The elements designator `/<slot-name>` is used to designate a slot
of an object, from which elements should be retrieved.

## descendants ##

Example:

        #$object//attr1
{:cl:}

The descendants designator `//<slot-name>` is used to designate a slot
in an object or one of its descendants. 
A breadth-first search is used to locate the slot with name `<slot-name>`
from which the elements should be retrieved.
(How about termination, etc ..)

## predicate block ##

Example:

         #$object/attr1[@type=some-class]
{:cl:}

A predicate used to select elements from a slot.
Predicates are delimited by square brackets.

## symbolic-expression block ##

Example:

         #${(car lst)}/attr1 
          => (slot-value (car lst) 'attr1)
{:cl:}

A symbolic-expression delimited by curly brackets that is evaluated to obtain a 
value that is used in the query expression.


The Parser
---------

The AST produced by the parser has the following form:

        (path (<root-node> (<ast-nodes>)))
{:cl:}

The *root-node* of the AST is the symbol of the object that is queried, 
or the symbolic-expression block that calculates this object.

*AST-Nodes* are represented by lists.
They have the form: 

     (<token-type> <attribute> ...)
{:cl:}

*Attributes* can be other *AST-Nodes*, which allows arbitrary (linear) nesting of nodes, 
and thus the creation of a tree.

Known Limitations
-----------------

Expressions of the form 

            (setf #$object/attr1/attr2 value)
{:cl:}
will not work as expected, i.e. set the value of the slot `attr2`.

The reason for this behavior is that the reader-macro `#$` creates a path expression
that expands into a select program.

However, it should be possible to write a custom `setf`-function that generates the correct 
path expression:
     
     (setf #$object/attr1/attr2 value)
      => #$object/attr1[{(setf (slot-value @value 'attr2) value)}]
{:cl:}


Path Expressions
----------------

# Primitives #

## Symbolic Expression Block ##

Syntax:

        symbolic-expression-block ::= {<symbolic-expression>}
{:syntax:}

Symbolic expression blocks contain symbolic-expressions, 
which are evaluated by common-lisp in the lexical scope of the *compiled query*.

      [[{<symbolic-expression>}]] -> (eval <symbolic-expression>)
{:syntax:}
 

## Queried Object ##

Syntax:

        queried-object ::=  ( <symbol> | <symbolic-expression-block> )
{:syntax:}

A path expression starts with a *symbol* containing an object, or a *symbolic-expression-block*
that when evaluated returns an object.
The conditions that must hold in evaluating symbols and symbolic expression blocks 
are more specifically:

     [[symbol]] -> (or (typep symbol 'standard-object) (typep symbol 'sequence))
     [[<symbolic-expression-block>]] -> (or (typep <symbolic-expression-block> 'standard-object) (typep <symbolic-expression-block> 'sequence))
{:syntax:}

## Predicate Block ##

Syntax:
      
      predicate-block ::= [<predicate-expression>]

Predicate blocks are used to select elements in path expressions.

## Predicate Expressions ##

Syntax:

        predicate-expression ::= ( <keyword> | <symbolic-expression-block> | ^( <keyword> | <symbolic-expression-block> ) )
{:syntax:}

When the predicate-expression is a keyword, the following rules apply:

     * if the keyword is of the form `:symbol-name`, then the *predicate-expression*
       matches the queried-object, when the slot is defined and bound
     * if the keyword is of the form `:number`, then the *predicate-expression*
       matches the nth element when the queried object is a sequence, otherwise
       the queried object is not matched

When the predicate-expression is a symbolic-expression block, 
then the value of evaluating the symbolic-expression block is interpreted as *generalized-boolean*.
In case that evaluating symbolic-expression block returns a non-NIL value, the queried-object is matched,
otherwise not.

In all other cases, where the predicate-expression is neither a keyword nor a symbolic-expression block,
the queried object is compared to the value obtained by evaluating the predicate expression.
If the comparison was successful, then the queried-object is matched.

## Simple Query ##

Syntax:

        simple-query ::= <queried-object> ( <predicate-block> | <symbolic-expression> )?


A simple query returns either the queried-object, a simple-vector containing the queried-object, or an empty simple-vector.
When no predicate-block is used, then the query always returns a simple-vector containing the queried-object.

If a predicate-block is used that uses a predicate-expression of form `:number`, then the queried-object 
is treated like a sequence. That is, if the queried object is no sequence, 
it is treated as a sequence of one element containing this object.
If the index designated by the predicate-expression is greater than the length of the sequence denoted by queried-object,
then an empty vector is returned.

If any of the other types of predicate-blocks matches, then a vector containing the matched elements is returned.

Element Selection
-----------------

A path-query expression returns usually a *vector* of elements.
The only exception from this rule is *element-selection*.
Element selection is accomplished by a predicate-block containing 
a *fixnum* index as predicate-expression.

Example:

        #$object/attr1[0]
{:cl:}
The fixnum in the square brackets denotes the index in 
the *result-vector* of the query, which returns a single object.

# Examples #

        ? (defclass test-class () 
            ((attr1 :initarg :attr1) 
             (attr2 :initarg :attr2)))
        #<STANDARD-CLASS TEST-CLASS>             
        
        ? (defvar object 
                  (make-instance 'test-class 
                                 :attr1 "A" 
                                 :attr2 (make-instance 'test-class 
                                                       :attr1 "A" 
                                                       :attr2 #("B" "C"))))
        OBJECT

        ? #$object/attr1[:0] 
        "A"

        ? #$object/attr1[1=1]
        #("A")

        ? #$object/attr1[0=1]
        #()

        ? #$object/attr2[:attr1]
        #(#<TEST-CLASS #x300041BCB46D>)

        ? #$object/attr2[:attr3]
        #()

        ? #$object/attr2[:attr1="A"]
        #(#<TEST-CLASS #x300041BCB46D>)

        ? #$object/attr2[:attr1="B"]
        #()

        ? #$object/attr2/attr1[{@value}="A"]
        #("A")

        ? #$object/attr2/attr1["A"]
        #("A")

        ? (defvar val "A")
        VAL

        ? #$object/attr2/attr1[val]
        #("A")
{:cl:}

Be careful and do not mess-up symbols and keywords!

*Bad* Example:

        ? (defvar attr1 "A")
        ATTR1

        ? #$object/attr2/attr1[attr1]
        #("A")

        ? #$object/attr2[attr1]
        #()

        ? #$object/attr2[:attr1]
        #(#<TEST-CLASS #x300041BCB46D>)
{:cl:}