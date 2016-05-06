
Polymorphism {#polymorphism}
============

\begin{comment}
\begin{code}
{-@ LIQUID "--short-names"         @-}
{-@ LIQUID "--no-termination"      @-}
{-@ LIQUID "--scrape-used-imports" @-}

module VectorBounds
   ( safeLookup
   , unsafeLookup
   , vectorSum, vectorSum'
   , absoluteSum, absoluteSum'
   , dotProduct
   , sparseProduct, sparseProduct'
   , eeks
   , head, head', head''
   ) where

import Prelude      hiding (head, abs, length)
import Data.List    (foldl')
import Data.Vector  hiding (head, foldl')

absoluteSum'     :: Vector Int -> Int
dotProduct     :: Vector Int -> Vector Int -> Int
absoluteSum     :: Vector Int -> Int
sparseProduct, sparseProduct'  :: Vector Int -> [(Int, Int)] -> Int
\end{code}
\end{comment}

Refinement types shine when we want to establish
properties of *polymorphic* datatypes and higher-order
functions. Rather than be abstract, let's illustrate this
with a [classic][dmlarray] use-case.

\newthought{Array Bounds Verification} aims to ensure
that the indices used to retrieve values from an array are indeed
*valid* for the array, i.e. are between `0` and the
*size* of the array. For example, suppose we create
an `array` with two elements:

~~~~~{.spec}
twoLangs  = fromList ["haskell", "javascript"]
~~~~~

Lets attempt to look it up at various indices:

\begin{code}
eeks      = [ok, yup, nono]
  where
    ok    = twoLangs ! 0
    yup   = twoLangs ! 1
    nono  = twoLangs ! 3
\end{code}

If we try to *run* the above, we get a nasty shock: an
exception that says we're trying to look up `twoLangs`
at index `3` whereas the size of `twoLangs` is just `2`.

~~~~~{.sh}
Prelude> :l 03-poly.lhs
[1 of 1] Compiling VectorBounds     ( 03-poly.lhs, interpreted )
Ok, modules loaded: VectorBounds.
*VectorBounds> eeks
Loading package ... done.
"*** Exception: ./Data/Vector/Generic.hs:249 ((!)): index out of bounds (3,2)
~~~~~

\newthought{In a suitable Editor} e.g. Vim or Emacs,
or if you push the "play" button in the online demo,
you will literally see the error *without*
running the code. Lets see how LiquidHaskell
checks `ok` and `yup` but flags `nono`, and along
the way, learn how it reasons about *recursion*,
*higher-order functions*, *data types* and *polymorphism*.


Specification: Vector Bounds {#vectorbounds}
--------------------------------------------

First, let's see how to *specify* array bounds safety by *refining*
the types for the [key functions][vecspec] exported by `Data.Vector`,
i.e. how to

1. *define* the size of a `Vector`
2. *compute* the size of a `Vector`
3. *restrict* the indices to those that are valid for a given size.

<div class="toolinfo">

\newthought{Imports}
We can write specifications for imported modules -- for which we
*lack* the code -- either directly in the client's source file or
better, in `.spec` files which can be reused across multiple client
modules.

\newthought{Include} directories can be specified when checking a file.
Suppose we want to check some file `target.hs` that imports an external
dependency `Data.Vector`. We can write specifications for `Data.Vector`
inside `include/Data/Vector.spec` which contains:

~~~~~{.spec}
-- | Define the size
measure vlen :: Vector a -> Int

-- | Compute the size
assume length :: x:Vector a -> {v:Int | v = vlen x}

-- | Lookup at an index
assume (!) :: x:Vector a -> {v:Nat | v < vlen x} -> a
~~~~~
</div>

Using this new specification is now a simple matter of telling LiquidHaskell
to include this file:

~~~~~{.sh}
$ liquid -i include/ target.hs
~~~~~

LiquidHaskell ships with specifications for `Prelude`, `Data.List`,
and `Data.Vector` which it includes by default.

\newthought{Measures} are used to define *properties* of
Haskell data values that are useful for specification and
verification. Think of `vlen` as the *actual*
size of a `Vector` regardless of how the size was computed.

\newthought{Assumes} are used to *specify* types describing the semantics of
functions that we cannot verify e.g. because we don't have the code
for them. Here, we are assuming that the library function `Data.Vector.length`
indeed computes the size of the input vector. Furthermore, we are stipulating
that the lookup function `(!)` requires an index that is betwen `0` and the real
size of the input vector `x`.

\newthought{Dependent Refinements} are used to describe relationships
*between* the elements of a specification. For example, notice how the
signature for `length` names the input with the binder `x` that then
appears in the output type to constrain the output `Int`. Similarly,
the signature for `(!)` names the input vector `x` so that the index
can be constrained to be valid for `x`.  Thus, dependency lets us
write properties that connect *multiple* program values.

\newthought{Aliases} are extremely useful for defining
*abbreviations* for commonly occuring types. Just as we
enjoy abstractions when programming, we will find it
handy to have abstractions in the specification mechanism.
To this end, LiquidHaskell supports *type aliases*.
For example, we can define `Vector`s of a given size `N` as:

\begin{code}
{-@ type VectorN a N = {v:Vector a | vlen v == N} @-}
\end{code}

\noindent and now use this to type `twoLangs` above as:

\begin{code}
{-@ twoLangs :: VectorN String 2 @-}
twoLangs     = fromList ["haskell", "javascript"]
\end{code}

Similarly, we can define an alias for `Int` values
between `Lo` and `Hi`:

\begin{code}
{-@ type Btwn Lo Hi = {v:Int | Lo <= v && v < Hi} @-}
\end{code}

\noindent after which we can specify `(!)` as:

~~~~~{.spec}
(!) :: x:Vector a -> Btwn 0 (vlen x) -> a
~~~~~

Verification: Vector Lookup
---------------------------

Let's try write some functions to sanity check the specifications.
First, find the starting element -- or `head` of a `Vector`

\begin{code}
head     :: Vector a -> a
head vec = vec ! 0
\end{code}

When we check the above, we get an error:

~~~~~{.liquiderror}
     src/03-poly.lhs:127:23: Error: Liquid Type Mismatch
       Inferred type
         VV : Int | VV == ?a && VV == 0

       not a subtype of Required type
         VV : Int | VV >= 0 && VV < vlen vec

       In Context
         VV  : Int | VV == ?a && VV == 0
         vec : Vector a | 0 <= vlen vec
         ?a  : Int | ?a == (0  :  int)
~~~~~

\noindent LiquidHaskell is saying that `0` is *not* a valid index
as it is not between `0` and `vlen vec`. Say what? Well, what if
`vec` had *no* elements! A formal verifier doesn't
make *off by one* errors.

\newthought{To Fix} the problem we can do one of two things.

1. *Require* that the input `vec` be non-empty, or
2. *Return* an output if `vec` is non-empty, or

Here's an implementation of the first approach, where we define
and use an alias `NEVector` for non-empty `Vector`s

\begin{code}
{-@ type NEVector a = {v:Vector a | 0 < vlen v} @-}

{-@ head' :: NEVector a -> a @-}
head' vec = vec ! 0
\end{code}

<div class="hwex" id="Vector Head">
Replace the `undefined` with an *implementation* of `head''`
which accepts *all* `Vector`s but returns a value only when
the input `vec` is not empty.
</div>

\begin{code}
head''     :: Vector a -> Maybe a
head'' vec = undefined
\end{code}

<div class="hwex" id="Unsafe Lookup"> The function `unsafeLookup` is
a wrapper around the `(!)` with the arguments flipped. Modify the
specification for `unsafeLookup` so that the *implementation* is
accepted by LiquidHaskell.
</div>

\begin{code}
{-@ unsafeLookup :: Int -> Vector a -> a @-}
unsafeLookup index vec = vec ! index
\end{code}

<div class="hwex" id="Safe Lookup">
Complete the implementation of `safeLookup` by filling
in the implementation of `ok` so that it performs a bounds
check before the access.
</div>

\begin{code}
{-@ safeLookup :: Vector a -> Int -> Maybe a @-}
safeLookup x i
  | ok        = Just (x ! i)
  | otherwise = Nothing
  where
    ok        = undefined
\end{code}

Inference: Our First Recursive Function
---------------------------------------

Ok, let's write some code! Let's start with a recursive
function that adds up the values of the elements of an
`Int` vector.

\begin{code}
-- >>> vectorSum (fromList [1, -2, 3])
-- 2
vectorSum         :: Vector Int -> Int
vectorSum vec     = go 0 0
  where
    go acc i
      | i < sz    = go (acc + (vec ! i)) (i + 1)
      | otherwise = acc
    sz            = length vec
\end{code}

<div class="hwex" id="Guards">
What happens if you *replace* the guard with `i <= sz`?
</div>

<div class="hwex" id="Absolute Sum">
Write a variant of the above function that computes the
`absoluteSum` of the elements of the vector.
</div>

\begin{code}
-- >>> absoluteSum (fromList [1, -2, 3])
-- 6
{-@ absoluteSum :: Vector Int -> Nat @-}
absoluteSum     = undefined
\end{code}


\newthought{Inference}
LiquidHaskell verifies `vectorSum` -- or, to be precise,
the safety of the vector accesses `vec ! i`. The verification
works out because LiquidHaskell is able to
*automatically infer* ^[In your editor, click on `go` to see the inferred type.]

~~~~~{.spec}
go :: Int -> {v:Int | 0 <= v && v <= sz} -> Int
~~~~~

\noindent which states that the second parameter `i` is
between `0` and the length of `vec` (inclusive). LiquidHaskell
uses this and the test that `i < sz` to establish that `i` is
between `0` and `(vlen vec)` to prove safety.

<div class="hwex" id="Off by one?">
Why does the type of `go` have `v <= sz` and not `v < sz` ?
</div>

Higher-Order Functions: Bottling Recursion in a `loop`
------------------------------------------------------

Let's refactor the above low-level recursive function
into a generic higher-order `loop`.

\begin{code}
loop :: Int -> Int -> a -> (Int -> a -> a) -> a
loop lo hi base f =  go base lo
  where
    go acc i
      | i < hi    = go (f i acc) (i + 1)
      | otherwise = acc
\end{code}

We can now use `loop` to implement `vectorSum`:

\begin{code}
vectorSum'      :: Vector Int -> Int
vectorSum' vec  = loop 0 n 0 body
  where
    body i acc  = acc + (vec ! i)
    n           = length vec
\end{code}

\newthought{Inference} is a convenient option. LiquidHaskell finds:

~~~~~{.spec}
loop :: lo:Nat -> hi:{Nat|lo <= hi} -> a -> (Btwn lo hi -> a -> a) -> a
~~~~~

\noindent In english, the above type states that

- `lo` the loop *lower* bound is a non-negative integer
- `hi` the loop *upper* bound is a greater than `lo`,
- `f`  the loop *body* is only called with integers between `lo` and `hi`.

\noindent
It can be tedious to have to keep typing things like the above.
If we wanted to make `loop` a public or exported function, we
could use the inferred type to generate an explicit signature.

At the call `loop 0 n 0 body` the parameters `lo` and `hi` are
instantiated with `0` and `n` respectively, which, by the way
is where the inference engine deduces non-negativity.
Thus LiquidHaskell concludes that `body` is only called with
values of `i` that are *between* `0` and `(vlen vec)`, which
verifies the safety of the call `vec ! i`.

<div class="hwex" id="Using Higher-Order Loops">
Complete the implementation of `absoluteSum'` below.
When you are done, what is the type that is inferred for `body`?
</div>

\begin{code}
-- >>> absoluteSum' (fromList [1, -2, 3])
-- 6
{-@ absoluteSum' :: Vector Int -> Nat @-}
absoluteSum' vec = loop 0 n 0 body
  where
    n            = length vec
    body i acc   = undefined
\end{code}

<div class="hwex" id="Dot Product">
The following uses `loop` to compute
`dotProduct`s. Why does LiquidHaskell flag an error?
Fix the code or specification so that LiquidHaskell
accepts it. </div>

\vspace{1.0in}

\begin{code}
-- >>> dotProduct (fromList [1,2,3]) (fromList [4,5,6])
-- 32
{-@ dotProduct :: x:Vector Int -> y:Vector Int -> Int @-}
dotProduct x y = loop 0 sz 0 body
  where
    sz         = length x
    body i acc = acc + (x ! i)  *  (y ! i)
\end{code}

Refinements and Polymorphism {#sparsetype}
----------------------------------------

While the standard `Vector` is great for *dense* arrays,
often we have to manipulate *sparse* vectors where most
elements are just `0`. We might represent such vectors
as a list of index-value tuples:

\begin{code}
{-@ type SparseN a N = [(Btwn 0 N, a)] @-}
\end{code}

\noindent Implicitly, all indices *other* than those in the list
have the value `0` (or the equivalent value for the type `a`).

\newthought{The Alias} `SparseN` is just a
shorthand for the (longer) type on the right, it does not
*define* a new type. If you are familiar with the *index-style*
length encoding e.g. as found in [DML][dml] or [Agda][agdavec],
then note that despite  appearances, our `Sparse` definition
is *not* indexed.

\newthought{Sparse Products}
Let's write a function to compute a sparse product

\begin{code}
{-@ sparseProduct  :: x:Vector _ -> SparseN _ (vlen x) -> _ @-}
sparseProduct x y   = go 0 y
  where
    go n ((i,v):y') = go (n + (x!i) * v) y'
    go n []         = n
\end{code}

LiquidHaskell verifies the above by using the specification
to conclude that for each tuple `(i, v)` in the list `y`, the
value of `i` is within the bounds of the vector `x`, thereby
proving `x ! i` safe.

\newthought{Folds}
The sharp reader will have undoubtedly noticed that the sparse product
can be more cleanly expressed as a [fold][foldl]:

~~~~~{.spec}
foldl' :: (a -> b -> a) -> a -> [b] -> a
~~~~~

\noindent We can simply fold over the sparse vector, accumulating the `sum`
as we go along

\begin{code}
{-@ sparseProduct'  :: x:Vector _ -> SparseN _ (vlen x) -> _ @-}
sparseProduct' x y  = foldl' body 0 y
  where
    body sum (i, v) = sum + (x ! i)  * v
\end{code}

\noindent
LiquidHaskell digests this without difficulty.
The main trick is in how the polymorphism of
`foldl'` is instantiated.

1. GHC infers that at this site, the type variable `b` from the
   signature of `foldl'` is instantiated to the Haskell type `(Int, a)`.

2. Correspondingly, LiquidHaskell infers that in fact `b`
   can be instantiated to the *refined* `(Btwn 0 v (vlen x), a)`.

Thus, the inference mechanism saves us a fair bit of typing and
allows us to reuse existing polymorphic functions over containers
and such without ceremony.

Recap
-----


This chapter gave you an idea of how one can use refinements
to verify size related properties, and more generally, to
specify and verify properties of recursive and polymorphic
functions. Next, let's see how we can use LiquidHaskell to
prevent the creation of illegal values by refining data
type definitions.
