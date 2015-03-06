Logic & SMT Solvers 101
=======================


Steal from 291 lecture notes

Syntax
------


(C)onstants
-----------

~~~~~{.haskell}
    c := 0, 1, 2, ...
~~~~~

(V)ariables
-----------

~~~~~{.haskell}
    v := x, y, z, ...
~~~~~


(E)xpressions
-------------

~~~~~{.haskell}
    e := v                      -- variable
       | c                      -- constant
       | (e + e)                -- addition
       | (e - e)                -- subtraction
       | (c * e)                -- cmultiplication by constant
       | (v e1 e2 ... en)       -- uninterpreted function application
       | (if p then e else e)   -- if-then-else
~~~~~

(R)elations
-----------

~~~~~{.haskell}
    r := ==               -- equality
       | /=               -- disequality
       | >=               -- greater than or equal
       | <=               -- less than or equal
       | >                -- greater than
       | <                -- less than
~~~~~


(P)redicates
------------

    p := (e r e)          -- binary relation
       | (v e1 e2 ... en) -- predicate (or alias) application
       | (p && p)         -- and
       | (p || p)         -- or
       | (p => p)         -- implies
       | (not p)          -- negation
       | true
       | false



~~~~~{.haskell}
Expressions

e := 

Predicates

p := true
   | false
   | p1 && p2
   | p1 || p2
   | p1 => p2
   | p1 <=> p2
   | e
~~~~~


Semantics (Validity & Satisfaction)
-----------------------------------

+ Atoms (arith)

  Example

+ Connectives (and or not implies)

  Examples

+ UIFs ()

