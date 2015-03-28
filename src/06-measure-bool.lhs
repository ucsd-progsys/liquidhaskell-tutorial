
Boolean Measures {#boolmeasures}
================


In the last two chapters, we saw how refinements could be used to
reason about the properties of basic `Int` values like vector
indices, or the elements of a list. Next, lets see how we can
describe properties of aggregate structures like lists and trees,
and use these properties to improve the APIs for operating over
such structures.

\begin{comment}
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}


module Measures where

import Prelude hiding(foldr, foldr1, map, sum, head, tail, null)

main = putStrLn "Hello"

-- | Old Definitions

{-@ type Nat     = {v:Int | 0 <= v} @-}
{-@ type Pos     = {v:Int | 0 <  v} @-}
{-@ type NonZero = {v:Int | 0 /= v} @-}

{-@ die :: {v:_ | false} -> a @-}
die msg = error msg

-- Type Definitions
divide     :: Int -> Int -> Int
size1, size2 :: [a] -> Int
\end{code}
\end{comment}


Partial Functions 
------------------

As a motivating example, let us return to the problem of ensuring
the safety of division. Recall that we wrote:

\begin{code}
{-@ divide :: Int -> NonZero -> Int @-}
divide _ 0 = die "divide-by-zero"
divide x n = x `div` n
\end{code}

\newthought{The Precondition} asserted by the input type `NonZero`
allows LiquidHaskell to prove that the `die` is *never* executed at
run-time, but consequently, requires us to establish that wherever
`divide` is *used*, the second parameter be provably non-zero.
This requirement is not onerous when we know what the
divisor is *statically*

\begin{code}
avg2 x y   = divide (x + y)     2

avg3 x y z = divide (x + y + z) 3
\end{code}

\noindent However, it can be more of a challenge when the divisor
is obtained *dynamically*. For example, lets write a function to
find the number of elements in a list

\begin{code}
size        :: [a] -> Int
size []     =  0
size (_:xs) =  1 + size xs
\end{code}

\noindent and use it to compute the average value of a list:

\begin{code}
avgMany xs = divide total elems 
  where
    total  = sum  xs
    elems  = size xs
\end{code}

Uh oh. LiquidHaskell wags its finger at us! 

~~~~~{.liquiderror}
     src/04-measure.lhs:77:27-31: Error: Liquid Type Mismatch
       Inferred type
         VV : Int | VV == elems
      
       not a subtype of Required type
         VV : Int | 0 /= VV
      
       In Context
         VV    : Int | VV == elems
         elems : Int
~~~~~

\newthought{We cannot prove} that the divisor is `NonZero`,
because it *can be* `0` -- when the list is *empty*. Thus, we
need a way of specifying that the input to `avgMany` is indeed
non-empty!

Lifting Functions to Measures {#usingmeasures}
-----------------------------

\newthought{How} shall we tell LiquidHaskell that a list is *non-empty*?
Recall the notion of `measure` previously [introduced](#vectorbounds)
to describe the size of a `Data.Vector`. In that spirit, lets write
a function that computes whether a list is not empty:

\begin{code}
notEmpty       :: [a] -> Bool 
notEmpty []    = False
notEmpty (_:_) = True 
\end{code}

\newthought{A measure} is a *total* Haskell function,

1. With a *single* equation per data constructor, and 
2. Guaranteed to *terminate*, typically via structural recursion.

\noindent
We can tell LiquidHaskell to *lift* a function meeting
the above requirements into the refinement logic by declaring:

\begin{code}
{-@ measure notEmpty @-}
\end{code}


\newthought{Non-Empty Lists} can now be described as
the *subset* of plain old Haskell lists `[a]` for which
the predicate `notEmpty` holds

\begin{code}
{-@ type NEList a = {v:[a] | notEmpty v} @-}
\end{code}

We can now refine various signatures to establish the safety of
the list-average function.

\newthought{Size} returns a non-zero value *if* the input list is
not-empty. We capture this condition with an [implication](#semantics)
in the output refinement.

\begin{code}
{-@ size :: xs:[a] -> {v:Nat | notEmpty xs => v > 0} @-}
\end{code}

\newthought{Average} is only sensible for non-empty lists.
Happily, we can specify this using the refined `NEList` type:

\begin{code}
{-@ average :: NEList Int -> Int @-}
average xs = divide total elems
  where
    total  = sum xs
    elems  = size xs
\end{code}

<div class="hwex" id="Average, Maybe">
Fix the code below to obtain an alternate variant
`average'` that returns `Nothing` for empty lists:
</div>

\begin{code}
average'      :: [Int] -> Maybe Int
average' xs
  | ok        = Just $ divide (sum xs) elems 
  | otherwise = Nothing 
  where
    elems     = size xs
    ok        = elems > 0 -- What expression goes here? 
\end{code}

<div class="hwex" id="Debugging Specifications">
An important aspect of formal verifiers like LiquidHaskell
is that they help establish properties not just of your *implementations*
but equally, or more importantly, of your *specifications*. In that spirit,
can you explain why the following two variants of `size` are *rejected*
by LiquidHaskell? 
</div>

\begin{code}
{-@ size1    :: xs:NEList a -> Pos @-}
size1 []     =  0
size1 (_:xs) =  1 + size1 xs

{-@ size2    :: xs:[a] -> {v:Int | notEmpty xs => v > 0} @-}
size2 []     =  0
size2 (_:xs) =  1 + size2 xs
\end{code}


A Safe List API 
---------------

Now that we can talk about non-empty lists, we can ensure
the safety of various list-manipulating functions which
are only well-defined on non-empty lists and crash otherwise.

\newthought{Head and Tail} are two of the canonical *dangerous*
functions, that only work on non-empty lists, and burn horribly
otherwise. We can type them simple as:


\begin{code}
{-@ head    :: NEList a -> a @-}
head (x:_)  = x
head []     = die "Fear not! 'twill ne'er come to pass"

{-@ tail    :: NEList a -> [a] @-}
tail (_:xs) = xs
tail []     = die "Relaxeth! this too shall ne'er be"
\end{code}

LiquidHaskell uses the precondition to deduce that
the second equations are *dead code*. Of course, this
requires us to establish that *callers* of `head` and `tail`
only invoke the respective functions with non-empty lists.

<div class="hwex" id="Safe Head">
Write down a specification for `null` such that `safeHead`
is verified. Do *not* force `null` to only take non-empty inputs,
that defeats the purpose. Instead, it's type should say that it
works on *all* lists and returns `True` *if and only if* the input
is non-empty.
</div>

\hint You may want to refresh your memory about implies `==>`
and `<=>` from the [chapter on logic](#semantics).

\begin{code}
safeHead      :: [a] -> Maybe a
safeHead xs
  | null xs   = Nothing
  | otherwise = Just $ head xs

{-@ null      :: [a] -> Bool @-}
null []       =  True
null (_:_)    =  False
\end{code}

\newthought{Groups}
Lets use the above to write a function that chunks sequences
into non-empty groups of equal elements:

\begin{code}
{-@ groupEq    :: (Eq a) => [a] -> [NEList a] @-}
groupEq []     = []
groupEq (x:xs) = (x:ys) : groupEq zs
  where
    (ys, zs)   = span (x ==) xs
\end{code}

\noindent By using the fact that *each element* in the
output returned by `groupEq` is in fact of the form `x:ys`,
LiquidHaskell infers that `groupEq` returns a `[NEList a]`
that is, a list of *non-empty lists*.

\newthought{To Eliminate Stuttering} from a string, we can use `groupEq`
to split the string into blocks of repeating `Char`s, and then just
extract the first `Char` from each block:

\begin{code}
-- >>> eliminateStutter "ssstringssss liiiiiike thisss"
-- "strings like this"
eliminateStutter xs = map head $ groupEq xs
\end{code}

\noindent
LiquidHaskell automatically instantiates the type parameter
for `map` in `eliminateStutter` to `notEmpty v` to deduce that
`head` is only called on non-empty lists.

\newthought{Foldr1} is one of my favorite folds; it uses
the first element of the sequence as the initial value.
Of course, it should only be called with non-empty sequences!

\begin{code}
{-@ foldr1         :: (a -> a -> a) -> NEList a -> a @-} 
foldr1 f (x:xs)    = foldr f x xs
foldr1 _ []        = die "foldr1" 

foldr              :: (a -> b -> b) -> b -> [a] -> b 
foldr _ acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
\end{code}

\newthought{To Sum} a non-empty list of numbers, we can just
perform a `foldr1` with the `+` operator:
Thanks to the precondition, LiquidHaskell will prove that
the `die` code is indeed dead. Thus, we can write

\begin{code}
{-@ sum :: (Num a) => NEList a -> a  @-}
sum []  = die "cannot add up empty list"
sum xs  = foldr1 (+) xs
\end{code}

\noindent Consequently, we can only invoke `sum` on non-empty lists, so:

\begin{code}
sumOk  = sum [1,2,3,4,5]    -- is accepted by LH, but

sumBad = sum []             -- is rejected by LH
\end{code}

<div class="hwex" id="Weighted Average">
The function below computes a weighted average of its input.
Unfortunately, LiquidHaskell is not very happy about it. Can you figure out
why, and fix the code or specification appropriately?
</div>

\begin{code}
{-@ wtAverage :: NEList (Pos, Pos) -> Int @-}
wtAverage wxs = divide totElems totWeight 
  where
    elems     = map (\(w, x) -> w * x) wxs
    weights   = map (\(w, _) -> w    ) wxs
    totElems  = sum elems
    totWeight = sum weights
    sum       = foldr1 (+)

map           :: (a -> b) -> [a] -> [b]
map _ []      =  []
map f (x:xs)  =  f x : map f xs
\end{code}

\hint On what variables are the errors? How are those variables' values computed?
Can you think of a better specification for the function(s) doing those computations?

<div class="hwex" id="Mitchell's Risers">
Non-empty lists pop up in many places, and it is rather convenient
to have the type system track non-emptiness without having to make
up special types. Consider the `risers` function, popularized
by [Neil Mitchell][mitchell-riser]. `safeSplit` requires
its input be non-empty; but LiquidHaskell believes that the
call inside `risers` fails this requirement. Fix the
specification for `risers` so that it is verified.
</div>

\begin{code}
{-@ risers   :: (Ord a) => xs:[a] -> {v: [[a]] | notEmpty xs => notEmpty v} @-}
risers           :: (Ord a) => [a] -> [[a]]
risers []        = []
risers [x]       = [[x]]
risers (x:y:etc)
  | x <= y       = (x:s) : ss
  | otherwise    = [x] : (s : ss)
    where
      (s, ss)    = safeSplit $ risers (y:etc)

{-@ safeSplit    :: NEList a -> (a, [a]) @-}
safeSplit (x:xs) = (x, xs)
safeSplit _      = die "don't worry, be happy"
\end{code}

Recap
-----

In this chapter we saw how LiquidHaskell lets you 

1. *Define* structural properties of data types, 

2. *Use refinements* over these properties to describe key
   invariants that establish, at compile-time, the safety
   of operations that might otherwise fail on unexpected
   values at run-time, all while,

3. *Working with plain Haskell types*, here, Lists,
   without having to [make up new types][apple-riser]
   which can have the unfortunate effect of adding
   a multitude of constructors and conversions which
   often clutter implementations and specifications.

\noindent 
Of course, we can do a lot more with measures, so lets press on!



