Escape Analysis
===============

Conditions for an escape:
 * assignments to class-variables escape
 * references used as method-arguments escape
 * references returned escape

the references r1, r2, and r3 escape in the following case:

r1.<foo.Bar: fooMethod(<baz>, <blubb>)>(r2,r3)
