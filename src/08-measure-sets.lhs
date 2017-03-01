

Elemental Measures {#setmeasure}
================


\begin{comment}
\begin{code}
{-@ LIQUID "--no-termination" @-}

module Sets where
import Data.Set hiding (insert, partition, filter, split, elems)
import Prelude  hiding (elem, reverse, filter)

main :: IO ()
main = return ()

{-@ die :: {v:_ | false} -> a @-}
die msg = error msg

isUnique, isNotUnique :: [Int]
mergeSort :: (Ord a) => [a] -> [a]
range :: Int -> Int -> [Int]
-- FIXME
{-@ predicate In X Xs      = Set_mem X Xs            @-}
{-@ predicate Subset X Y   = Set_sub X Y             @-}
{-@ predicate Empty  X     = Set_emp X               @-}
{-@ predicate Inter X Y Z  = X = Set_cap Y Z         @-}
{-@ predicate Union X Y Z  = X = Set_cup Y Z         @-}
{-@ predicate Union1 X Y Z = Union X (Set_sng Y) Z   @-}
{-@ predicate Disjoint X Y = Inter (Set_empty 0) X Y @-}
type List a = [a]
\end{code}
\end{comment}

Often, correctness requires us to reason about the *set of elements*
represented inside a data structure, or manipulated by a function.
Examples of this abound: for example, we'd like to know that:

+ *sorting* routines return permutations of their inputs --
  i.e. return collections whose elements are the same as the
  input set,

+ *resource* management functions do not inadvertently
  create duplicate elements or drop elements from set
  of tracked resources.

+ *syntax-tree* manipulating procedures create well-scoped
  trees where the set of used variables are contained
  within the set of variables previously defined.

\newthought{SMT Solvers} support very expressive logics.
In addition to linear arithmetic and uninterpreted functions,
they can [efficiently decide][smt-set] formulas over sets.
Next, lets see how LiquidHaskell lets us exploit this fact
to develop types and interfaces that guarantee invariants
over the set of elements of a structures.

Talking about Sets
------------------

First, we need a way to talk about sets in the refinement logic. We could
roll our own special Haskell type but for now, lets just use the `Set a`
type from the prelude's `Data.Set`.^[See [this](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2013/03/26/talking-about-sets.lhs/)
for a brief description of how to work directly with the set operators natively
supported by LiquidHaskell.]

\newthought{LiquidHaskell Lifts} the basic set operators from `Data.Set`
into the refinement logic. That is, the prelude defines the following
*logical* functions that correspond to the *Haskell* functions of the
same name:

~~~~~{.spec}
measure empty        :: Set a
measure singleton    :: a -> Set a
measure member       :: a -> Set a -> Bool
measure union        :: Set a -> Set a -> Set a
measure intersection :: Set a -> Set a -> Set a
measure difference   :: Set a -> Set a -> Set a
~~~~~

\newthought{Interpreted Operators}
The above operators are *interpreted* by the SMT solver.
That is, just like the SMT solver "knows", via the
axioms of the theory of arithmetic that:
$$x = 2 + 2 \Rightarrow x = 4$$
is a valid formula, i.e. holds for all $x$,
the solver "knows" that:
$$x = \tsng{1} \Rightarrow y = \tsng{2} \Rightarrow x = \tcap{x}{\tcup{y}{x}}$$
This is because, the above formulas belong to
a decidable Theory of Sets reduces to McCarthy's
more general [Theory of Arrays][mccarthy]. ^[See [this recent paper][z3cal]
to learn how modern SMT solvers prove equalities like the above.]


Proving QuickCheck Style Properties {#quickcheck}
-----------------------------------

To get the hang of whats going on, lets do a few warmup exercises,
using LiquidHaskell to prove various simple theorems about sets
and operations over them.

\newthought{We Refine The Set API} to make it easy to write down
theorems. That is, we give the operators in `Data.Set` refinement
type signatures that precisely track their set-theoretic behavior:

~~~~~{.spec}
empty        :: {v:Set a | v = empty}
member       :: x:a
             -> s:Set a
             -> {v:Bool | v <=> member x s}

singleton    :: x:a -> {v:Set a | v = singleton x}

union        :: x:Set a
             -> y:Set a
             -> {v:Set a | v = union x y}

intersection :: x:Set a
             -> y:Set a
             -> {v:Set a | v = intersection x y}

difference   :: x:Set a
             -> y:Set a
             -> {v:Set a | v = difference x y}
~~~~~

\newthought{We Can Assert Theorems} as [QuickCheck](quickcheck) style
*properties*, that is, as functions from arbitrary inputs to a `Bool`
output that must always be `True`. Lets define aliases for the the
`Bool`eans that are always `True` or `False`

\begin{code}
{-@ type True  = {v:Bool |     v} @-}
{-@ type False = {v:Bool | not v} @-}
\end{code}

\noindent We can use `True` to state theorems.
For example, the unexciting arithmetic equality above becomes:

\begin{code}
{-@ prop_one_plus_one_eq_two :: _ -> True @-}
prop_one_plus_one_eq_two x   = (x == 1 + 1) `implies` (x == 2)
\end{code}

\noindent Where `implies` is just the implication function over ``Bool``

\begin{code}
{-@ implies        :: p:Bool -> q:Bool -> Implies p q  @-}
implies False _    = True
implies _     True = True
implies _    _     = False
\end{code}

\noindent and `Implies p q` is defined as

\begin{code}
{-@ type Implies P Q = {v:_ | v <=> (P => Q)} @-}
\end{code}

<div class="hwex" id="Bounded Addition">
Write and prove a QuickCheck style theorem that:
$\forall x, y. x < 100 \wedge y < 100 \Rightarrow x + y < 200$.
</div>

\begin{code}
{-@ prop_x_y_200 :: _ -> _ -> True @-}
prop_x_y_200 x y = False -- fill in the theorem body
\end{code}


\newthought{The Commutativity of Intersection} can be easily
stated and proved as a QuickCheck style theorem:

\begin{code}
{-@ prop_intersection_comm :: _ -> _ -> True @-}
prop_intersection_comm x y
  = (x `intersection` y) == (y `intersection` x)
\end{code}

\newthought{The Associativity of Union} can similarly be confirmed:

\begin{code}
{-@ prop_intersection_comm :: _ -> _ -> True @-}
prop_union_assoc x y z
  = (x `union` (y `union` z)) == (x `union` y) `union` z
\end{code}

\newthought{The Distributivity Laws} for Boolean Algebra can
be verified by writing properties over the relevant operators.
For example, we lets check that `union` distributes over `intersection`:

\begin{code}
{-@ prop_intersection_dist :: _ -> _ -> _ -> True @-}
prop_intersection_dist x y z
  =  x `intersection` (y `union` z)
     ==
     (x `intersection` y) `union` (x `intersection` z)
\end{code}

\newthought{Non-Theorems} should be rejected.
So, while we're at it, let's make sure LiquidHaskell
doesn't prove anything that *isn't* true ...

\begin{code}
{-@ prop_cup_dif_bad :: _ -> _ -> True @-}
prop_cup_dif_bad x y
  = pre `implies` (x == ((x `union` y) `difference` y))
  where
    pre = True  -- Fix this with a non-trivial precondition
\end{code}

<div class="hwex" id="Set Difference">
Why does the above property fail?

1. Use QuickCheck (or your own little grey cells) to
   find a *counterexample* for the property `prop_cup_dif_bad`.

2. Use the counterexample to assign `pre` a non-trivial
   (i.e. other than `False`) condition so that the property
   can be proved.
</div>

Thus, LiquidHaskell's refined types offer a nice interface
for interacting with the SMT solvers in order to *prove*
theorems, while letting us use QuickCheck to generate
counterexamples.^[The [SBV][sbv] and [Leon][leon] projects
describe a different DSL based approach for using SMT solvers
from Haskell and Scala respectively.]

Content-Aware List API {#listelems}
----------------------------------

Lets return to our real goal, which is to to verify
properties of programs. First, we need a way to refine
the list API to precisely track the set of elements
in a list.


\newthought{The Elements of a List} can be described by
a simple recursive measure that walks over the list, building
up the set:

\begin{code}
{-@ measure elts @-}
elts        :: (Ord a) => [a] -> Set a
elts []     = empty
elts (x:xs) = singleton x `union` elts xs
\end{code}

\noindent
Lets write a few helpful aliases for various refined lists that will
then make the subsequent specifications pithy and crisp.

+ A list with elements `S`

\begin{code}
{-@ type ListS a S = {v:[a] | elts v = S} @-}
\end{code}

+ An *empty* list

\begin{code}
{-@ type ListEmp a = ListS a {Set_empty 0} @-}
\end{code}

+ A list whose contents *equal* those of list `X`

\begin{code}
{-@ type ListEq a X = ListS a {elts X}    @-}
\end{code}

+ A list whose contents are a *subset* of list `X`

\begin{code}
{-@ type ListSub a X = {v:[a]| Set_sub (elts v) (elts X)} @-}
\end{code}

+ A list whose contents are the union of lists `X` and `Y`

\begin{code}
{-@ type ListUn a X Y = ListS a {Set_cup (elts X) (elts Y)} @-}
\end{code}

+ A list whose contents are exactly `X` and the contents of `Y`

\begin{code}
{-@ type ListUn1 a X Y = ListS a {Set_cup (Set_sng X) (elts Y)} @-}
\end{code}

\newthought{The Measures strengthens} the data constructors for lists. That is
we get the automatically refined types for "nil" and "cons":

~~~~~{.spec}
data List a where
  []  :: ListEmp a
  (:) :: x:a -> xs:List a -> ListUn1 a x xs
~~~~~

\begin{comment}
\noindent Here, the predicates correspond to various primitive
relations over `Set`s that are natively implemented within the
SMT solver:

~~~~~{.spec}
-- FIXME
predicate In X Y       = -- X is an element of Y
predicate Subset X Y   = -- X is a subset of Y
predicate Disjoint X Y = -- X and Y are Disjoint
predicate Empty X      = -- X is empty
predicate Union X Y Z  = -- X is the union of Y and Z
predicate Union1 X Y Z = -- X is the union of {Y} and Z
~~~~~
\end{comment}

Lets take our new vocabulary out for a spin!

\newthought{The Append} function returns a list whose elements are the *union*
of the elements of the input Lists:

\begin{code}
{-@ append'       :: xs:_ -> ys:_ -> ListUn a xs ys @-}
append' []     ys = ys
append' (x:xs) ys = x : append' xs ys
\end{code}

<div class="hwex" id="Reverse">
Write down a type for `revHelper` so that `reverse'` is verified by LiquidHaskell.
</div>

\begin{code}
{-@ reverse' :: xs:List a -> ListEq a xs @-}
reverse' xs = revHelper [] xs

revHelper acc []     = acc
revHelper acc (x:xs) = revHelper (x:acc) xs
\end{code}

<div class="hwex" id="Halve">
\singlestar Write down a specification for `halve` such
that the subsequent "theorem" `prop_halve_append` is
proved by LiquidHaskell.
</div>

\begin{code}
halve            :: Int -> [a] -> ([a], [a])
halve 0 xs       = ([], xs)
halve n (x:y:zs) = (x:xs, y:ys) where (xs, ys) = halve (n-1) zs
halve _ xs       = ([], xs)

{-@ prop_halve_append  :: _ -> _ -> True @-}
prop_halve_append n xs = elts xs == elts xs'
  where
    xs'      =  append' ys zs
    (ys, zs) =  halve n xs
\end{code}

\hint You may want to remind yourself about the
*dimension-aware* signature for `partition` from
[the earlier chapter](#listreducing).

<div class="hwex" id="Membership">
Write down a signature for `elem` that suffices to verify
`test1` and `test2`.
</div>

\begin{code}
{-@ elem      :: (Eq a) => a -> [a] -> Bool @-}
elem x (y:ys) = x == y || elem x ys
elem _ []     = False

{-@ test1 :: True @-}
test1      = elem 2 [1, 2, 3]

{-@ test2 :: False @-}
test2      = elem 2 [1, 3]
\end{code}

Permutations
------------

Next, lets use the refined list API to prove that
various sorting routines return *permutations*
of their inputs, that is, return output lists whose
elements are the *same as* those of the input lists.^[Since we are focusing on the elements, lets not
distract ourselves with the [ordering invariant](#orderedlists)
and reuse plain old lists. See [this](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2013/07/29/putting-things-in-order.lhs/)
for how to specify and verify order with plain old lists.]

\newthought{Insertion Sort} is the simplest of all the
list sorting routines; we build up an (ordered) output
list `insert`ing each element of the input list into
the appropriate position of the output:

\begin{code}
insert x []     = [x]
insert x (y:ys)
  | x <= y      = x : y : ys
  | otherwise   = y : insert x ys
\end{code}

\noindent Thus, the output of `insert` has all the
elements of the input `xs`, plus the new element `x`:

\begin{code}
{-@ insert :: x:a -> xs:List a -> ListUn1 a x xs @-}
\end{code}

\noindent The above signature lets us prove that the output
of the sorting routine indeed has the elements of the input:

\begin{code}
{-@ insertSort :: (Ord a) => xs:List a -> ListEq a xs @-}
insertSort []     = []
insertSort (x:xs) = insert x (insertSort xs)
\end{code}

<div class="hwex" id="Merge">
Fix the specification of `merge` so that the subsequent property
`prop_merge_app` is verified by LiquidHaskell.
</div>

\begin{code}
{-@ merge :: xs:List a -> ys:List a -> List a @-}
merge (x:xs) (y:ys)
  | x <= y           = x : merge xs (y:ys)
  | otherwise        = y : merge (x:xs) ys
merge [] ys          = ys
merge xs []          = xs

{-@ prop_merge_app   :: _ -> _ -> True   @-}
prop_merge_app xs ys = elts zs == elts zs'
  where
    zs               = append' xs ys
    zs'              = merge   xs ys
\end{code}


<div class="hwex" id="Merge Sort">
\doublestar Once you write the correct type
for `merge` above, you should be able to prove the
er, unexpected signature for `mergeSort` below.

1. Make sure you are able verify the given signature.

2. Obviously we don't want `mergeSort` to return the empty list,
   so there's a bug. Find and fix it, so that you *cannot*
   prove that the output is empty, but *can* instead prove
   that the output is `ListEq a xs`.
</div>

\begin{code}
{-@ mergeSort :: (Ord a) => xs:List a -> ListEmp a @-}
mergeSort []  = []
mergeSort xs  = merge (mergeSort ys) (mergeSort zs)
  where
   (ys, zs)   = halve mid xs
   mid        = length xs `div` 2
\end{code}

Uniqueness
----------

Often, we want to enforce the invariant that a particular collection
contains *no duplicates*; as multiple copies in a collection of file
handles or system resources can create unpleasant leaks.
For example, the [XMonad][xmonad] window manager creates a
sophisticated *zipper* data structure to hold the list of
active user windows and carefully maintains the invariant
that that the zipper contains no duplicates. Next, lets see how to
specify and verify this invariant using LiquidHaskell, first for
lists, and then for a simplified zipper.

\newthought{To Specify Uniqueness} we need a way of saying that a
list has *no duplicates*. There are many ways to do so; the
simplest is a *measure*:

\begin{code}
{-@ measure unique @-}
unique        :: (Ord a) => List a -> Bool
unique []     = True
unique (x:xs) = unique xs && not (member x (elts xs))
\end{code}

\noindent We can use the above to write an alias for duplicate-free lists

\begin{code}
{-@ type UList a = {v:List a | unique v }@-}
\end{code}

\noindent Lets quickly check that the right lists are indeed `unique`

\begin{code}
{-@ isUnique    :: UList Int @-}
isUnique        = [1, 2, 3]        -- accepted by LH

{-@ isNotUnique :: UList Int @-}
isNotUnique     = [1, 2, 3, 1]     -- rejected by LH
\end{code}

\newthought{The Filter} function returns a *subset* of
its elements, and hence, *preserves* uniqueness. That is,
if the input is unique, the output is too:

\begin{code}
{-@ filter   :: (a -> Bool)
             -> xs:UList a
             -> {v:ListSub a xs | unique v}
  @-}
filter _ []   = []
filter f (x:xs)
  | f x       = x : xs'
  | otherwise = xs'
  where
    xs'       = filter f xs
\end{code}

<div class="hwex" id="Filter">
It seems a bit draconian to require that `filter` only
be called with unique lists. Write down a more permissive
type for `filter'` below such that the subsequent uses
are verified by LiquidHaskell.
</div>

\begin{code}
filter' _ []   = []
filter' f (x:xs)
  | f x       = x : xs'
  | otherwise = xs'
  where
    xs'       = filter' f xs

{-@ test3 :: UList _ @-}
test3     = filter' (> 2) [1,2,3,4]

{-@ test4 :: [_] @-}
test4     = filter' (> 3) [3,1,2,3]
\end{code}

<div class="hwex" id="Reverse">
\singlestar When we `reverse` their order, the set of elements
is unchanged, and hence unique (if the input was unique).
Why does LiquidHaskell reject the below? Can you fix things
so that we can prove that the output is a `UList a`?
</div>

\begin{code}
{-@ reverse    :: xs:UList a -> UList a    @-}
reverse         = go []
  where
    {-@ go     :: a:List a -> xs:List a -> List a @-}
    go a []     = a
    go a (x:xs) = go (x:a) xs
\end{code}

\newthought{The Nub} function constructs a `unique` list from
an arbitrary input by traversing the input and tossing out
elements that are already `seen`:

\begin{code}
{-@ nub              :: List a -> UList a @-}
nub xs                = go [] xs
  where
    go seen []        = seen
    go seen (x:xs)
      | x `isin` seen = go seen     xs
      | otherwise     = go (x:seen) xs
\end{code}

\noindent The key membership test is done by `isin`,
whose output is `True` exactly when the element is
in the given list. ^[Which should be clear by now,
if you did a certain exercise above \ldots.]

\begin{code}
-- FIXME
{-@ predicate In X Xs = Set_mem X (elts Xs) @-}

{-@ isin :: x:_ -> ys:_ -> {v:Bool | v <=> In x ys }@-}
isin x (y:ys)
  | x == y    = True
  | otherwise = x `isin` ys
isin _ []     = False
\end{code}

<div class="hwex" id="Append">
\singlestar Why does `append`ing two `UList`s not
return a `UList`? Fix the type signature below so
that you can prove that the output is indeed `unique`.
</div>

\begin{code}
{-@ append       :: UList a -> UList a -> UList a @-}
append []     ys = ys
append (x:xs) ys = x : append xs ys
\end{code}

<div class="hwex" id="Range">
\doublestar `range i j` returns the list of `Int`
between `i` and `j`. LiquidHaskell refuses to
acknowledge that the output is indeed a `UList`.
Fix the code so that LiquidHaskell verifies that
it implements the given signature (and of course,
computes the same result.)
</div>

\begin{code}
{-@ type Btwn I J = {v:_ | I <= v && v < J} @-}

{-@ range     :: i:Int -> j:Int -> UList (Btwn i j) @-}
range i j
  | i < j     = i : range (i + 1) j
  | otherwise = []
\end{code}

\hint This may be easier to do *after* you read this chapter [about lemmas](#lemmas).


Unique Zippers
--------------

A [zipper](wiki-zipper) is an aggregate data stucture
that is used to arbitrarily traverse the structure and
update its contents. For example, a zipper for a list is
a data type that contains an element (called `focus`)
that we are currently `focus`-ed on, a list of elements
to the `left` of (i.e. before) the focus, and a list of
elements to the `right` (i.e. after) the focus.


\begin{code}
data Zipper a = Zipper {
    focus  :: a
  , left   :: List a
  , right  :: List a
  }
\end{code}

\newthought{XMonad} is a wonderful tiling window manager, that uses
a [zipper][xmonad-stackset] to store the set of windows being managed.
Xmonad requires the crucial invariant that the values in the zipper
be unique, that is, be free of duplicates.

\newthought{We Refine Zipper} to capture the requirement
that legal zippers are unique. To this end, we state that
the `left` and `right` lists are unique, disjoint, and do
not contain `focus`.

\begin{code}
{-@ data Zipper a = Zipper {
      focus :: a
    , left  :: {v: UList a | not (In focus v)}
    , right :: {v: UList a | not (In focus v) && Disj v left }
    } @-}

{-@ predicate Disj X Y = Disjoint (elts X) (elts Y)        @-}
\end{code}

\newthought{Our Refined Zipper Constructor} makes *illegal states unrepresentable*.
That is, by construction, we will ensure that every `Zipper` is free of duplicates.
For example, it is straightforward to create a valid `Zipper` from a `unique` list:

\begin{code}
{-@ differentiate    :: UList a -> Maybe (Zipper a) @-}
differentiate []     = Nothing
differentiate (x:xs) = Just $ Zipper x [] xs
\end{code}

<div class="hwex" id="Deconstructing Zippers">
\singlestar Dually, the elements of a unique zipper
tumble out into a unique list. Strengthen the types of
`reverse` and `append` above so that LiquidHaskell
accepts the below signatures for `integrate`:
</div>

\begin{code}
{-@ integrate            :: Zipper a -> UList a @-}
integrate (Zipper x l r) = reverse l `append` (x : r)
\end{code}

\newthought{We can Shift the Focus} element to the left or
right while preserving the uniqueness invariant. Here's the
code that shifts the focus to the left:

\begin{code}
focusLeft                      :: Zipper a -> Zipper a
focusLeft (Zipper t (l:ls) rs) = Zipper l ls (t:rs)
focusLeft (Zipper t [] rs)     = Zipper x xs []
  where
    (x:xs)                     = reverse (t:rs)
\end{code}

\noindent To shift to the right, we simply *reverse*
the elements and shift to the left:

\begin{code}
focusRight    :: Zipper a -> Zipper a
focusRight    = reverseZipper . focusLeft . reverseZipper

reverseZipper :: Zipper a -> Zipper a
reverseZipper (Zipper t ls rs) = Zipper t rs ls
\end{code}

\newthought{To Filter} elements from a zipper, we need to
take care when the `focus` itself, or all the elements get
eliminated. In the latter case, there is no `Zipper` and so
the operation returns a `Maybe`:

\begin{code}
filterZipper :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
filterZipper p (Zipper f ls rs)
  = case filter p (f:rs) of
      f':rs' -> Just $ Zipper f' (filter p ls) rs'
      []     -> case filter p ls of
                  f':ls' -> Just $ Zipper f' ls' []
                  []     -> Nothing
\end{code}

\noindent Thus, by using LiquidHaskell's refinement types, and the SMT solvers
native reasoning about sets, we can ensure the key uniqueness invariant holds
in the presence of various tricky operations that are performed over `Zipper`s.

Recap
-----

In this chapter, we saw how SMT solvers can let us reason precisely about
the actual *contents* of data structures, via the theory of sets. In particular,
we saw how to:

* *Lift set-theoretic primitives* to refined Haskell functions from
  the `Data.Set` library,

* *Define measures* like `elts` that characterize the set of elements
  of structures, and `unique` that describe high-level application
  specific properties about those sets,

* *Specify and verify* that implementations enjoy various functional
  correctness properties, e.g. that sorting routines return permutations
  of their inputs, and various zipper operators preserve uniqueness.

Next, we present a variety of longer *case-studies* that illustrate
the techniques developed so far on particular application domains.
