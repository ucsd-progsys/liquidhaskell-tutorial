
Logic & SMT
===========

\begin{comment}
\begin{code}
{-@ LIQUID "--short-names" @-}

module Logic where
main :: IO ()
main = return ()


{-@ size  :: xs:[a] -> {v:Int | v = size xs} @-}

ax1 :: Int -> Bool
ax2 :: Int -> Bool
ax3 :: Int -> Int -> Bool
ax4 :: Int -> Int -> Bool
ax5 :: Int -> Int -> Int -> Bool
ax6 :: Int -> Int -> Bool

ex1, ex2 :: Bool -> Bool
ex3, ex3', ex4, ex6, ex7, exDeMorgan1, exDeMorgan2 :: Bool -> Bool -> Bool

infixr 9 ==>

{-@ invariant {v:[a] | size v >= 0} @-}
{-@ f :: x:Int -> {v:Int | v = f x} @-}
f :: Int -> Int
f = undefined
\end{code}
\end{comment}

As we shall see shortly, a refinement type is:

 > *Refinement Types* = *Types* + *Logical Predicates*

Let us begin by quickly recalling what we mean by "logical predicates"
in the remainder of this tutorial. ^[If you are comfortable with this material,
e.g. if you know what the "S", "M" and "T" stand for in SMT, and what QF-UFLIA
stands for (i.e. the quantifier free theory of linear arithmetic and
uninterpreted functions), then feel free skip to the next chapter.]
To this end, we will describe *syntax*, that is, what predicates *look*
like, and *semantics*, which is a fancy word for what predicates *mean*.

Syntax
------

A *logical predicate* is, informally speaking, a Boolean valued term drawn
from a *restricted* subset of Haskell. In particular, the expressions are
drawn from the following grammar comprising *constants*, *expressions* and
*predicates*.

\newthought{A Constant}^[When you see := you should read it as "is defined
to be"] `c` is simply one of the numeric values:

~~~~~{.spec}
    c := 0, 1, 2, ...
~~~~~

\newthought{A Variable} `v` is one of `x`, `y`, `z`, etc., these will refer
to (the values of) binders in our source programs.

~~~~~{.spec}
    v := x, y, z, ...
~~~~~

\newthought{An Expression} `e` is one of the following forms;
that is, an expression is built up as linear arithmetic expressions
over variables and constants and uninterpreted function applications.

~~~~~{.spec}
    e := v                   -- variable
       | c                   -- constant
       | e + e               -- addition
       | e - e               -- subtraction
       | c * e               -- linear multiply
       | v e1 e2 ... en      -- uninterpreted function application
~~~~~

\newthought{Examples of Expressions} include the following:

+ `x + y - z`
+ `2 * x`
+ `1 + size x`

\newthought{A Relation} is one of the usual (arithmetic)
comparison operators:

~~~~~{.spec}
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
`||` (or), `==>` (implies ^[Read `p ==> q` as "if `p` then `q`"]), `<=>` (if and only
if ^[Read `p <=> q` as "if `p` then `q` **and** if `q` then `p`"]), and `not`.

~~~~~{.spec}
    p := true
       | false
       | e r e           -- atomic binary relation
       | v e1 e2 ... en  -- predicate application
       | p  && p         -- and
       | p  || p         -- or
       | p ==> p         -- implies
       | p <=> p         -- if and only if
       | not p           -- negation
~~~~~


\newthought{Examples of Predicates} include the following:

+ `x + y <= 3`
+ `null x`
+ `x < 10 ==> y < 10 ==> x + y < 20`
+ `0 < x + y <=> 0 < y + x`

Semantics {#semantics}
----------------------

The syntax of predicates tells us what they *look* like, that is, what we
can *write down* as valid predicates. Next, let us turn our attention to
what a predicate *means*. Intuitively, a predicate is just a Boolean valued
Haskell function with `&&`, `||`, `not` being the usual operators and `==>` and
`<=>` being two special operators.

\newthought{The Implication} operator `==>` is equivalent to the following
Haskell function. (For now, ignore the signature: it just says the output
is a `Bool` that is equal to the *logical* implication between the inputs `p`
and `q`.)

\begin{code}
{-@ (==>) :: p:Bool -> q:Bool -> {v:Bool | v <=> (p ==> q)} @-}
False ==> False = True
False ==> True  = True
True  ==> True  = True
True  ==> False = False
\end{code}

\newthought{The If-and-only-if} operator `<=>` is equivalent to the
Haskell function:^[An observant reader may notice that <=> is the same as
== if the arguments are of type Bool]

\begin{code}
{-@ (<=>) :: p:Bool -> q:Bool -> {v:Bool | v <=> (p <=> q)} @-}
False <=> False = True
False <=> True  = False
True  <=> True  = True
True  <=> False = False
\end{code}

\newthought{An Environment} is a mapping from variables to their
Haskell types. For example, let `G` be an environment defined as

~~~~~{.spec}
    x :: Int
    y :: Int
    z :: Int
~~~~~

\noindent
which maps each variable `x`, `y` and `z` to the type `Int`.


\newthought{An Assignment} under an environment, is a mapping
from variables to values of the type specified in the environment.
For example,

~~~~~{.spec}
    x := 1
    y := 2
    z := 3
~~~~~

\noindent
is an assignment under `G` that maps `x`, `y` and `z` to the `Int` values
`1`, `2` and `3` respectively.

\newthought{A Predicate Evaluates} to either `True` or `False` under a given
assignment. For example, the predicate

~~~~~{.spec}
    x + y > 10
~~~~~

\noindent
evaluates to `False` given the above assignment but evaluates to `True`
under the assignment

~~~~~{.spec}
    x := 10
    y := 10
    z := 20
~~~~~


\newthought{A Predicate is Satisfiable} in an environment if *there exists*
an assignment (in that environment) that makes the predicate evaluate to `True`.
For example, in `G` the predicate

~~~~~{.spec}
    x + y == z
~~~~~

\noindent
is satisfiable, as the above assignment makes the predicate
evaluate to `True`.

\newthought{A Predicate is Valid} in an environment if *every*
assignment in that environment makes the predicate evaluate to
`True`. For example, the predicate

~~~~~{.spec}
    x < 10 || x == 10 || x > 10
~~~~~

\noindent
is valid under `G` as no matter what value we assign to `x`, the
above predicate will evaluate to `True`.


Verification Conditions
-----------------------

LiquidHaskell works without actually *executing* your
programs. Instead, it checks that your program meets the given
specifications in roughly two steps.

1. First, LH combines the code and types down to a set of
   *Verification Conditions* (VC) which are predicates that
   are valid *only if* your program satisfies a given
   property. ^[The process is described at length
   in [this paper][liquidpldi08]]

2. Next, LH *queries* an [SMT solver][smt-wiki] to determine
   whether these VCs are valid. If so, it says your program
   is *safe* and otherwise it *rejects* your program.

\newthought{The SMT Solver decides} whether a predicate (VC) is valid
*without enumerating* and evaluating all assignments. Indeed, it is
impossible to do so as there are usually infinitely many assignments
once the predicates refer to integers or lists and so on.  Instead,
the SMT solver uses a variety of sophisticated *symbolic algorithms*
to deduce whether a predicate is valid or not. This
process is the result of decades of work in mathematical logic and
decision procedures; the [Ph.D thesis of Greg Nelson][nelson-thesis]
is an excellent place to learn more about these beautiful algorithms.

\newthought{We Restrict the Logic} to ensure that all our VC queries
fall within the *decidable fragment*. This makes LiquidHaskell
extremely automatic -- there is *no* explicit manipulation of proofs,
just the specification of properties via types and of course, the
implementation via Haskell code!  This automation comes at a price:
all our refinements *must* belong to the logic above. Fortunately,
with a bit of creativity, we can say a *lot* in this logic. ^[In
particular, we will use the uninterpreted functions to create many
sophisticated abstractions.]

Examples: Propositions
----------------------

Finally, lets conclude this quick overview with some
examples of predicates, in order to build up our own
intuition about logic and validity.
Each of the below is a predicate from our refinement
logic. However, we write them as raw Haskell expressions
that you may be more familiar with right now, and so that
we can start to use LiquidHaskell to determine whether a
predicate is indeed valid or not.

\newthought{Let `TRUE` be a refined type} for `Bool`
valued expressions that *always* evaluate to `True`.
Similarly, we can define `FALSE` for `Bool` valued
expressions that *always* evaluate to `False`:^[This syntax will be discussed in
greater detail in [soon](#propositions)]

\begin{code}
{-@ type TRUE  = {v:Bool | v    } @-}
{-@ type FALSE = {v:Bool | not v} @-}
\end{code}

\noindent
Thus, a *valid predicate* is one that has the type
`TRUE`. The simplest example of a valid predicate
is just `True`:

\begin{code}
{-@ ex0 :: TRUE @-}
ex0 = True
\end{code}

\noindent of course, `False` is *not valid*

\begin{code}
{-@ ex0' :: TRUE @-}
ex0' = False
\end{code}

We can get more interesting predicates if we use variables.
For example, the following is valid predicate says that a
`Bool` variable is either `True` or `False`.

\begin{code}
{-@ ex1 :: Bool -> TRUE @-}
ex1 b = b || not b
\end{code}

\noindent Of course, a variable cannot be both `True`
and `False`, and so the below predicate is valid:

\begin{code}
{-@ ex2 :: Bool -> FALSE @-}
ex2 b = b && not b
\end{code}

The next few examples illustrate the `==>` operator.
You should read `p ==> q` as *if* `p` is true *then* `q`
must also be true.  Thus, the below predicates are valid
as if both `a` and `b` are true, then well, `a` is true,
and `b` is true.

\begin{code}
{-@ ex3 :: Bool -> Bool -> TRUE @-}
ex3 a b = (a && b) ==> a

{-@ ex4 :: Bool -> Bool -> TRUE @-}
ex4 a b = (a && b) ==> b
\end{code}

<div class="hwex" id="Implications and Or">
Of course, if we replace the `&&` with `||` the result is *not valid*.
Can you shuffle the variables around -- *without changing the operators* --
to make the formula valid?
</div>

\begin{code}
{-@ ex3' :: Bool -> Bool -> TRUE @-}
ex3' a b = (a || b) ==> a
\end{code}

The following predicates are valid because they encode
[modus ponens](http://en.wikipedia.org/wiki/Modus_ponens):
if you know that `a` implies `b` and you know that `a` is
true, then it must be the case that `b` is also true:

\begin{code}
{-@ ex6 :: Bool -> Bool -> TRUE @-}
ex6 a b = (a && (a ==> b)) ==> b

{-@ ex7 :: Bool -> Bool -> TRUE @-}
ex7 a b = a ==> (a ==> b) ==> b
\end{code}

Recall that `p <=> q` (read `p` if and only iff `q`) evaluates to `True`
exactly when `p` and `q` evaluate to the *same* values (`True` or `False`).
It is used to encode *equalities* between predicates. For example, we can
write down [De Morgan's laws](http://en.wikipedia.org/wiki/De_Morgan's_laws)
as the valid predicates:

\begin{code}
{-@ exDeMorgan1 :: Bool -> Bool -> TRUE @-}
exDeMorgan1 a b = not (a || b) <=> (not a && not b)
\end{code}

<div class="hwex" id="DeMorgan's Law">
The following version of DeMorgan's law is wrong.
Can you fix it to get a valid formula?
</div>

\begin{code}
{-@ exDeMorgan2 :: Bool -> Bool -> TRUE @-}
exDeMorgan2 a b = not (a && b) <=> (not a && not b)
\end{code}

Examples: Arithmetic
--------------------

Next, lets look at some predicates involving arithmetic.
The simplest ones don't have any variables, for example:

\begin{code}
{-@ ax0 :: TRUE @-}
ax0 = 1 + 1 == 2
\end{code}

\noindent Again, a predicate that evaluates to `False`
is *not* valid:

\begin{code}
{-@ ax0' :: TRUE @-}
ax0' = 1 + 2 == 2
\end{code}

\newthought{SMT Solvers determine Validity} *without*
enumerating assignments. For example, consider the
predicate:

\begin{code}
{-@ ax1 :: Int -> TRUE @-}
ax1 x = x < x + 1
\end{code}

\noindent It is trivially valid; as via the usual
laws of arithmetic, it is equivalent to `0 < 1`
which is `True` independent of the value of `x`.
The SMT solver is able to determine this validity
without enumerating the infinitely many possible
values for `x`. This kind of validity checking
lies at the heart of LiquidHaskell.

\newthought{We can combine arithmetic and propositional}
operators, as shown in the following examples:

\begin{code}
{-@ ax2 :: Int -> TRUE @-}
ax2 x = (x < 0) ==> (0 <= 0 - x)

{-@ ax3 :: Int -> Int -> TRUE @-}
ax3 x y = (0 <= x) ==> (0 <= y) ==> (0 <= x + y)

{-@ ax4 :: Int -> Int -> TRUE @-}
ax4 x y = (x == y - 1) ==> (x + 2 == y + 1)

{-@ ax5 :: Int -> Int -> Int -> TRUE @-}
ax5 x y z =   (x <= 0 && x >= 0)
          ==> (y == x + z)
          ==> (y == z)
\end{code}

<div class="hwex" id="Addition and Order">
The formula below is *not* valid. Do you know why?
Change the *hypothesis* i.e. the thing to the left
of the `==>` to make it a valid formula.
</div>

\begin{code}
{-@ ax6 :: Int -> Int -> TRUE @-}
ax6 x y = True ==> (x <= x + y)
\end{code}

Examples: Uninterpreted Function
--------------------------------

We say that function symbols are *uninterpreted* in the refinement logic,
because the SMT solver does not "know" how functions are defined. Instead,
the only thing that the solver knows is the *axiom of congruence* which
states that any function `f`, returns equal outputs when invoked on equal
inputs.

Let us define an uninterpreted function from `Int` to `Int`:

\begin{code}
{-@ measure f :: Int -> Int @-}
\end{code}

\newthought{We Test the Axiom of Congruence} by checking that the
following predicate
is valid:

\begin{code}
{-@ congruence :: Int -> Int -> TRUE @-}
congruence x y = (x == y) ==> (f x == f y)
\end{code}

\noindent Again, remember we are *not evaluating* the code above;
indeed we *cannot* evaluate the code above because we have no
definition of `f`. Still, the predicate is valid as the
congruence axiom holds for any possible interpretation
of `f`.

Here is a fun example; can you figure out why this
predicate is indeed valid? (The SMT solver can...)

\begin{code}
{-@ fx1 :: Int -> TRUE @-}
fx1 x =   (x == f (f (f x)))
      ==> (x == f (f (f (f (f x)))))
      ==> (x == f x)
\end{code}

To get a taste of why uninterpreted functions will prove useful
lets write a function to compute the `size` of a list:

\begin{code}
{-@ measure size @-}
size        :: [a] -> Int
size []     = 0
size (x:xs) = 1 + size xs
\end{code}

We can now verify that the following predicates are *valid*:

\begin{code}
{-@ fx0 :: [a] -> [a] -> TRUE @-}
fx0 xs ys = (xs == ys) ==> (size xs == size ys)
\end{code}

\noindent Note that to determine that the above is valid, the SMT
solver does not need to know the *meaning* or *interpretation* of
`size` -- merely that it is a function. When we need some information
about the definition, of `size` we will put it inside the predicate.
For example, in order to prove that the following is valid:

\begin{code}
{-@ fx2 :: a -> [a] -> TRUE @-}
fx2 x xs = 0 < size ys
  where
    ys   = x : xs
\end{code}

\noindent LiquidHaskell actually asks the SMT solver to
prove the validity of a VC predicate which states that
sizes are non-negative and that since `ys` equals `x:xs`,
the size of `ys` is one more than `xs`. ^[Fear not! We
will describe how this works [soon](#autosmart)]

\begin{code}
{-@ fx2VC :: _ -> _ -> _ -> TRUE @-}
fx2VC x xs ys =   (0 <= size xs)
              ==> (size ys == 1 + size xs)
              ==> (0 < size ys)
\end{code}

Recap
-----

This chapter describes exactly what we, for the purposes of this book,
mean by the term *logical predicate*.

1. We defined a grammar -- a restricted subset of Haskell corresponding
   to `Bool` valued expressions.
2. The restricted grammar lets us use SMT solvers to decide whether
   a predicate is *valid* that is, evaluates to `True` for *all* values
   of the variables.
3. Crucially, the SMT solver determins validity *without enumerating*
   and evaluating the predicates (which would take forever!) but instead
   by using clever symbolic algorithms.

Next, lets see how we can use logical predicates to *specify* and
*verify* properties of real programs.
