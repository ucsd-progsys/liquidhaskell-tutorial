A Quick Primer on Logic & SMT
=============================

\begin{comment}
\begin{code}
module Logic where
main :: IO ()
main = return ()
\end{code}
\end{comment}

As we shall see shortly, a refinement type is:

 > *Refinement Types* = *Types* + *Logical Predicates*

Let us begin by quickly recalling what we mean by "logical predicates"
in the remainder of this tutorial. ^[If you are comfortable with this material,
e.g. if you know what the "S", "M" and "T" stand for in SMT, and what QF-UFLIA
stands for i.e. the quantifier free theory of linear arithmetic and uninterpreted
functions, then feel free skip to the next chapter.]
To this end, we will describe *syntax*, that is, what predicates *look*
like, and *semantics*, which is a fancy word for what predicates *mean*.

Syntax
------

A *logical predicate* is, informally speaking, a Boolean valued term drawn
from a *restricted* subset of Haskell. In particular, the expressions are
drawn from the following grammar comprising *constants*, *expressions* and
*predicates*.

\newthought{A Constant} `c` is simply one of the numeric values:

~~~~~{.haskell}
    c := 0, 1, 2, ...
~~~~~

\newthought{A Variable} `v` is one of `x`, `y`, `z`, etc., these will refer
to (the values of) binders in our source programs.

~~~~~{.haskell}
    v := x, y, z, ...
~~~~~

\newthought{An Expression} `e` is one of the following forms;
that is, an expression is built up as linear arithmetic expressions
over variables and constants and uninterpreted function applications.

~~~~~{.haskell}
    e := v                   -- variable
       | c                   -- constant
       | e + e               -- addition
       | e - e               -- subtraction
       | c * e               -- linear multiply
       | v e1 e2 ... en      -- unint. func. appl.
~~~~~

\newthought{Examples of Expressions} include the following:

+ `x + y - z`
+ `2 * x`
+ `1 + size x`

\newthought{A Relation} is one of the usual (arithmetic)
comparison operators:

~~~~~{.haskell}
    r := ==               -- equality
       | /=               -- disequality
       | >=               -- greater than or equal
       | <=               -- less than or equal
       | >                -- greater than
       | <                -- less than
~~~~~

\newthought{A Predicate} is either an atomic predicate, obtained by comparing
two expressions, or, an application of a predicate function to a list of arguments,
or the Boolean combination of the above predicates with the operators `&&` (and),
`||` (or), `=>` (implies ^[Read `p => q` as "if `p` then `q`"]), `<=>` (if and only
if ^[Read `p <=> q` as "if `p` then `q` **and** if `q` then `p`"]), and `not`.

~~~~~{.haskell}
    p := true
       | false
       | e r e           -- atomic binary relation
       | v e1 e2 ... en  -- predicate application
       | p  && p         -- and
       | p  || p         -- or
       | p  => p         -- implies
       | p <=> p         -- if and only if
       | not p           -- negation
~~~~~


\newthought{Examples of Predicates} include the following:

+ `x + y <= 3`
+ `null x`
+ `x < 10 => y < 10 => x + y < 20`
+ `0 < x + y <=> 0 < y + x`

Semantics
---------

The syntax of predicates tells us what they *look* like, that is, what we
can *write down* as valid predicates. Next, let us turn our attention to
what a predicate *means*. Intuitively, a predicate is just a Boolean valued
Haskell function -- `&&`, `||`, `not` are the usual operators and `=>` and `<=>`
are two special operators. ^[Don't try to *actually* define these
as such in Haskell; they are reserved keywords and you will get
a syntax error]

\newthought{The Implication} operator `=>` is equivalent to the Haskell
function:

~~~~~{.haskell}
    (=>)  :: Bool -> Bool -> Bool
    False => False = True
    False => True  = True
    True  => True  = True
    True  => False = False
~~~~~

\newthought{The If-and-only-if} operator `<=>` is equivalent to the Haskell function:

~~~~~{.haskell}
    (<=>)  :: Bool -> Bool -> Bool
    False <=> False = True
    False <=> True  = False
    True  <=> True  = True
    True  <=> False = False
~~~~~


\newthought{An Environment} is a mapping from variables to their Haskell types.
For example, the environment `G` defined

~~~~~{.haskell}
    x :: Int
    y :: Int
    z :: Int
~~~~~

\noindent
maps each variable `x`, `y` and `z` to the type `Int`.


\newthought{An Assignment} under an environment, is a mapping from variables
to values of the type specified in the environment. For example,

~~~~~{.haskell}
    x := 1
    y := 2
    z := 3
~~~~~

\noindent
is an assignment under `G` that maps `x`, `y` and `z` to the `Int` values
`1`, `2` and `3` respectively.

\newthought{A Predicate Evaluates} to either `True` or `False` under a given
assignment. For example, the predicate

~~~~~{.haskell}
    x + y > 10
~~~~~

\noindent
evaluates to `False` given the above assignment but evaluates to `True`
under the assignment `x := 10, y := 10`.


\newthought{A Predicate is Satisfiable} in an environment if *there exists*
an assignment (in that environment) that makes the predicate evaluate to `True`.
For example, in `G` the predicate

~~~~~{.haskell}
    x + y == z
~~~~~

\noindent
is satisfiable, as the above assignment makes the predicate evaluate to `True`.

\newthought{A Predicate is Valid} in an environment if *every* assignment
in that environment makes the predicate evaluate to `True`. For example,
the predicate

~~~~~{.haskell}
x < 10 || x == 10 || x > 10
~~~~~

\noindent
is valid under `G` as no matter what value we assign to `x`, the above predicate
will evaluate to `True`.


Verification Conditions
-----------------------

LiquidHaskell works without actually *executing* your
programs. Instead, it checks that your program meets the given
specifications in roughly two steps.

1. First, LH combines the code and types down to a set of
   *Verification Conditions* (VC) which are predicates that are valid
   *only if* your program satisfies a given property. ^[The process is
   described at length in [this paper][liquidpldi08]]

2. Next, LH *queries* an [SMT solver][smt-wiki] to determine whether
   these VCs are valid. If so, it says your program is *safe* and 
   otherwise it *rejects* your program.

\newthought{We Carefully Restrict the Refinement Logic} so that the
SMT solver can *correctly answer* (or "decide") all the validity
queries.  That is, the SMT solver can determine that *all* assignments
will make a VC satisfiable, *without* actually enumerating the
assignments and evaluating the formula. If that sounds a bit
remarkable, it is: its the magic of mathematical logic and the
fruit of decades of research on [decision procedures][nelson-oppen].

HEREHERE

 *all* validity queries (i.e. VCs) that are s (and hence, validity queries)
Rather than getting into the formal details of how this mechanism works
 let
us illustrate it by example.

