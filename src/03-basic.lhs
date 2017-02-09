
Refinement Types
================

\begin{comment}
\begin{code}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--no-termination" @-}
module Intro where

import Prelude hiding (abs)
divide     :: Int -> Int -> Int
zero'''' :: Int
die     :: String -> a
\end{code}
\end{comment}

\newthought{What is a Refinement Type?} In a nutshell,

 > *Refinement Types* = *Types* + *Predicates*


\noindent
That is, refinement types allow us to decorate types with
*logical predicates*, which you can think of as *boolean-valued*
Haskell expressions, that constrain the set of values described
by the type. This lets us specify sophisticated invariants of
the underlying values.

Defining Types {#definetype}
--------------

Let us define some refinement types:^[You can read the type of `Zero` as:
"`v` is an `Int` *such that* `v` equals `0`" and `NonZero` as : "`v` is
an `Int` *such that* `v` does not equal `0`"]

\begin{code}
{-@ type Zero    = {v:Int | v == 0} @-}
{-@ type NonZero = {v:Int | v /= 0} @-}
\end{code}

\newthought{The Value Variable} `v` denotes the set of valid inhabitants
of each refinement type. Hence, `Zero` describes the *set of* `Int` values
that are equal to `0`, that is, the singleton set containing just `0`, and
`NonZero` describes the set of `Int` values that are *not* equal to `0`,
that is, the set `{1, -1, 2, -2, ...}` and so on.
^[We will use `@`-marked comments to write refinement
type annotations in the Haskell source file, making these
types, quite literally, machine-checked comments!]

\newthought{To use} these types we can write:

\begin{code}
{-@ zero :: Zero @-}
zero  = 0 :: Int

{-@ one, two, three :: NonZero @-}
one   = 1 :: Int
two   = 2 :: Int
three = 3 :: Int
\end{code}

Errors
------

If we try to say nonsensical things like:

\begin{code}
nonsense = one'
  where
  {-@ one' :: Zero @-}
  one' = 1 :: Int
\end{code}

\noindent
LiquidHaskell will complain with an error message:

~~~~~{.sh}
../liquidhaskell-tutorial/src/03-basic.lhs:72:3-6: Error: Liquid Type Mismatch

72 |   one' = 1 :: Int
       ^^^^

  Inferred type
    VV : {VV : Int | VV == (1 : int)}

  not a subtype of Required type
    VV : {VV : Int | VV == 0}
~~~~~

\noindent
The message says that the expression `1 :: Int` has the type

~~~~~{.sh}
    {v:Int | v == 1}
~~~~~

\noindent
which is *not* (a subtype of) the *required* type

~~~~~{.sh}
    {v:Int | v == 0}
~~~~~

\noindent
as `1` is not equal to `0`.

Subtyping
---------

What is this business of *subtyping*? Suppose we have some more refinements of `Int`

\begin{code}
{-@ type Nat   = {v:Int | 0 <= v}        @-}
{-@ type Even  = {v:Int | v mod 2 == 0 } @-}
{-@ type Lt100 = {v:Int | v < 100}       @-}
\end{code}

\newthought{What is the type of} `zero`? `Zero` of course, but also `Nat`:

\begin{code}
{-@ zero' :: Nat @-}
zero'     = zero
\end{code}

\noindent
and also `Even`:

\begin{code}
{-@ zero'' :: Even @-}
zero''     = zero
\end{code}

\noindent
and also any other satisfactory refinement,
such as ^[We use a different names `zero'`,
`zero''` etc. as (currently) LiquidHaskell
supports *at most* one refinement type
for each top-level name.]


\begin{code}
{-@ zero''' :: Lt100  @-}
zero'''     = zero
\end{code}

\newthought{Subtyping and Implication}
`Zero` is the most precise type for `0::Int`, as it is a *subtype* of `Nat`,
`Even` and `Lt100`. This is because the set of values defined by `Zero`
is a *subset* of the values defined by `Nat`, `Even` and `Lt100`, as
the following *logical implications* are valid:

+ $v = 0 \Rightarrow 0 \leq v$
+ $v = 0 \Rightarrow v \ \mbox{mod}\ 2 = 0$
+ $v = 0 \Rightarrow v < 100$

\begin{comment}
Chris Tetreault: I believe that this whole section (within this comment block)
can be eliminated. It should be clear to anybody who's still following at this
point that if I can make a small boolean expression (Nat, Even, Lt100), that
I can make a massive boolean expression (Nat && Even && Lt100). I fell that this
section just uses a bunch of math runes to confuse us mere mortals, but doesn't
really add much.

\newthought{Composing Refinements}
If $P \Rightarrow Q$ and $P \Rightarrow R$ then $P \Rightarrow Q \wedge R$.
Thus, when a term satisfies multiple refinements, we can compose those
refinements with `&&`:

\begin{code}
{-@ zero'''' :: {v:Int | 0 <= v && v mod 2 == 0 && v < 100} @-}
zero''''     = 0
\end{code}
\end{comment}

\begin{comment}
ES: this is confusingly worded
\end{comment}

\newthought{In Summary} the key points about refinement types are:

1. A refinement type is just a type *decorated* with logical predicates.
2. A term can have *different* refinements for different properties.
3. When we *erase* the predicates we get the standard Haskell types.^[Dually, a
standard Haskell type has the trivial refinement `true`. For example, `Int` is
equivalent to `{v:Int|true}`.]

Writing Specifications
----------------------

Let's write some more interesting specifications.

\newthought{Typing Dead Code} We can wrap the usual `error`
function in a function `die` with the type:

\begin{code}
{-@ die :: {v:String | false} -> a  @-}
die msg = error msg
\end{code}

The interesting thing about `die` is that the input type has the
refinement `false`, meaning the function must only be called with
`String`s that satisfy the predicate `false`.
This seems bizarre; isn't it *impossible* to satisfy `false`?
Indeed! Thus, a program containing `die` typechecks *only* when
LiquidHaskell can prove that `die` is *never called*. For example, LiquidHaskell will *accept*

\begin{code}
cannotDie = if 1 + 1 == 3
              then die "horrible death"
              else ()
\end{code}

\noindent
by inferring that the branch condition is always `False` and so `die`
cannot be called. However, LiquidHaskell will *reject*

\begin{code}
canDie = if 1 + 1 == 2
           then die "horrible death"
           else ()
\end{code}

\noindent
as the branch may (will!) be `True` and so `die` can be called.




Refining Function Types: Pre-conditions
--------------------------------------

Let's use `die` to write a *safe division* function that
*only accepts* non-zero denominators.

\begin{code}
divide'     :: Int -> Int -> Int
divide' n 0 = die "divide by zero"
divide' n d = n `div` d
\end{code}

From the above, it is clear to *us* that `div` is only
called with non-zero divisors. However, LiquidHaskell reports an
error at the call to `"die"` because, what if `divide'`
is actually invoked with a `0` divisor?

We can specify that will not happen, with a *pre-condition*
that says that the second argument is non-zero:

\begin{code}
{-@ divide :: Int -> NonZero -> Int @-}
divide _ 0 = die "divide by zero"
divide n d = n `div` d
\end{code}

\newthought{To Verify} that `divide` never calls `die`, LiquidHaskell infers
that `"divide by zero"` is not merely of type `String`, but in fact
has the the refined type `{v:String | false}` *in the context* in
which the call to `die` occurs. LiquidHaskell arrives at this conclusion by
using the fact that in the first equation for `divide` the
*denominator* is in fact

~~~~~{.sh}
    0 :: {v: Int | v == 0}
~~~~~

\noindent
which *contradicts* the pre-condition (i.e. input) type.
Thus, by contradition, LiquidHaskell deduces that the first equation is
*dead code* and hence `die` will not be called at run-time.

\newthought{Establishing Pre-conditions}
The above signature forces us to ensure that that when we
*use* `divide`, we only supply provably `NonZero` arguments.
Hence, these two uses of `divide` are fine:

\begin{code}
avg2 x y   = divide (x + y) 2
avg3 x y z = divide (x + y + z) 3
\end{code}

<div class="hwex" id="List Average">
Consider the function `avg`:

1. Why does LiquidHaskell flag an error at `n` ?
2. How can you change the code so LiquidHaskell verifies it?

</div>

\begin{code}
avg       :: [Int] -> Int
avg xs    = divide total n
  where
    total = sum xs
    n     = length xs
\end{code}


Refining Function Types: Post-conditions
---------------------------------------

Next, let's see how we can use refinements to describe the *outputs* of a
function. Consider the following simple *absolute value* function

\begin{code}
abs           :: Int -> Int
abs n
  | 0 < n     = n
  | otherwise = 0 - n
\end{code}

We can use a refinement on the output type to specify that the function
returns non-negative values

\begin{code}
{-@ abs :: Int -> Nat @-}
\end{code}

LiquidHaskell *verifies* that `abs` indeed enjoys the
above type by deducing that `n` is trivially non-negative
when `0 < n` and that in the `otherwise` case,
the value `0 - n` is indeed non-negative. ^[LiquidHaskell
is able to automatically make these arithmetic deductions
by using an [SMT solver][smt-wiki] which has built-in
decision procedures for arithmetic, to reason about the
logical refinements.]

Testing Values: Booleans and Propositions {#propositions}
-----------------------------------------

In the above example, we *compute* a value that is guaranteed to be a `Nat`.
Sometimes, we need to *test* if a value satisfies some property, e.g., is `NonZero`.
For example, let's write a command-line *calculator*:

\begin{code}
calc = do putStrLn "Enter numerator"
          n <- readLn
          putStrLn "Enter denominator"
          d <- readLn
          putStrLn (result n d)
          calc
\end{code}

\noindent which takes two numbers and divides them.
The function `result` checks if `d` is strictly positive
(and hence, non-zero), and does the division, or otherwise
complains to the user:

\begin{code}
result n d
  | isPositive d = "Result = " ++ show (n `divide` d)
  | otherwise    = "Humph, please enter positive denominator!"
\end{code}

Finally, `isPositive` is a test that returns a `True` if
its input is strictly greater than `0` or `False` otherwise:

\begin{code}
isPositive :: Int -> Bool
isPositive x = x > 0
\end{code}

\newthought{To verify} the call to `divide` inside `result`
we need to tell LiquidHaskell that the division only happens with a `NonZero`
value `d`. However, the non-zero-ness is established via the *test*
that occurs inside the guard `isPositive d`. Hence, we require a
*post-condition* that states that `isPositive` only returns `True`
when the argument is positive:

\begin{code}
{-@ isPositive :: x:Int -> {v:Bool | v <=> x > 0} @-}
\end{code}

In the above signature, the output type (post-condition)
states that `isPositive x` returns `True` if and only if
`x` was in fact strictly greater than `0`. In other words,
we can write post-conditions for plain-old `Bool`-valued *tests*
to establish that user-supplied values satisfy some desirable
property (here, `Pos` and hence `NonZero`) in order to then
safely perform some computation on it.

<div class="hwex" id="Propositions">
What happens if you *delete* the type for `isPositive` ?
Can you *change* the type for `isPositive` (i.e. write some other type)
while preserving safety?
</div>

<div class="hwex" id="Assertions">
Consider the following [assert][hoogle-assert] function, and two use sites.
Write a suitable refinement type signature for `lAssert` so that
`lAssert` and `yes` are accepted but `no` is rejected.
</div>

\begin{code}
{-@ lAssert  :: Bool -> a -> a @-}
lAssert True  x = x
lAssert False _ = die "yikes, assertion fails!"

yes = lAssert (1 + 1 == 2) ()
no  = lAssert (1 + 1 == 3) ()
\end{code}

\hint You need a pre-condition that `lAssert` is only called with `True`.

Putting It All Together
-----------------------

Let's wrap up this introduction with a simple `truncate` function
that connects all the dots.

\begin{code}
truncate :: Int -> Int -> Int
truncate i max
  | i' <= max' = i
  | otherwise  = max' * (i `divide` i')
    where
      i'       = abs i
      max'     = abs max
\end{code}

\noindent
The expression `truncate i n` evaluates to `i` when the absolute value
of `i` is less than the upper bound `max`, and otherwise *truncates* the value
at the maximum `n`. LiquidHaskell verifies that the use of `divide` is
safe by inferring that:

1. `max' < i'` from the branch condition,
2. `0 <= i'`   from the `abs` post-condition, and
3. `0 <= max'` from the `abs` post-condition.

From the above, LiquidHaskell infers that `i' /= 0`. That is, at the
call site `i' :: NonZero`, thereby satisfying the pre-condition
for `divide` and verifying that the program has no pesky
divide-by-zero errors.


Recap
-----

This concludes our quick introduction to Refinement Types and
LiquidHaskell. Hopefully you have some sense of how to

1. **Specify** fine-grained properties of values by decorating their
   types with logical predicates.
2. **Encode** assertions, pre-conditions, and post-conditions with suitable
   function types.
3. **Verify** semantic properties of code by using automatic logic engines
   (SMT solvers) to track and establish the key relationships between
   program values.
