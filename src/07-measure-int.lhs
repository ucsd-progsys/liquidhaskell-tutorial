Numeric Measures {#numericmeasure}
================

\begin{comment}
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}

module NumericMeasures where
import Prelude  hiding  (map, zipWith, zip, take, drop, reverse)

{-@ type TRUE = {v:Bool | v} @-}

{-@ die :: {v:_ | false} -> a @-}
die msg = error msg
take, drop, take' :: Int -> [a] -> [a]
txgo              :: Int -> Int -> Vector (Vector a) -> Vector (Vector a)
quickSort         :: (Ord a) => [a] -> [a]
size              :: [a] -> Int
flatten :: Int -> Int -> Vector (Vector a) -> Vector a

{-@ invariant {v:[a] | 0 <= size v} @-}
\end{code}
\end{comment}


Many of the programs we have seen so far, for example those in
[here](#vectorbounds), suffer from *indexitis*. This is a term
coined by [Richard Bird][bird-pearls] which describes a tendency
to perform low-level manipulations to iterate over the indices
into a collection, opening the door to various off-by-one
errors. Such errors can be eliminated by instead programming
at a higher level, using a [wholemeal approach][hinze-icfp09]
where the emphasis is on using aggregate operations, like `map`,
`fold` and `reduce`.

\newthought{Wholemeal programming is no panacea} as it still
requires us to take care when operating on *different* collections;
if these collections are *incompatible*, e.g. have the wrong dimensions,
then we end up with a fate worse than a crash, a possibly meaningless
result. Fortunately, LiquidHaskell can help. Lets see how we can use
measures to specify dimensions and create a dimension-aware API for
lists which can be used to implement wholemeal
dimension-safe APIs.^[In a [later chapter](#kmeans-case-study)
we will use this API to implement K-means clustering.]

Wholemeal Programming
---------------------

Indexitis begone! As an example of wholemeal programming, lets
write a small library that represents vectors as lists and
matrices as nested vectors:

\begin{code}
data Vector a = V { vDim  :: Int
                  , vElts :: [a]
                  }
              deriving (Eq)

data Matrix a = M { mRow  :: Int
                  , mCol  :: Int
                  , mElts :: Vector (Vector a)
                  }
              deriving (Eq)
\end{code}

\newthought{The Dot Product} of two `Vector`s can be easily computed using a fold:

\begin{code}
dotProd       :: (Num a) => Vector a -> Vector a -> a
dotProd vx vy = sum (prod xs ys)
  where
    prod      = zipWith (\x y -> x * y)
    xs        = vElts vx
    ys        = vElts vy
\end{code}

\newthought{Matrix Multiplication} can similarly be expressed
in a high-level, wholemeal fashion, by eschewing low level index
manipulations in favor of a high-level *iterator* over the `Matrix` elements:

\begin{code}
matProd       :: (Num a) => Matrix a -> Matrix a -> Matrix a
matProd (M rx _ xs) (M _ cy ys)
                 = M rx cy elts
  where
    elts         = for xs $ \xi ->
                     for ys $ \yj ->
                       dotProd xi yj
\end{code}

\newthought{The Iteration} embodied by the `for` combinator, is simply
a `map` over the elements of the vector.

~~~~~{.spec}
for            :: Vector a -> (a -> b) -> Vector b
for (V n xs) f = V n (map f xs)
~~~~~

\newthought{Wholemeal programming frees} us from having to fret
about low-level index range manipulation, but is hardly a panacea.
Instead, we must now think carefully about the *compatibility*
of the various aggreates. For example,

+ `dotProd` is only sensible on vectors of the same dimension;
  if one vector is shorter than another (i.e. has fewer elements)
  then we will won't get a run-time crash but instead will get
  some gibberish result that will be dreadfully hard to debug.

+ `matProd` is only well defined on matrices of compatible
  dimensions; the number of columns of `mx` must equal the
  number of rows  of `my`. Otherwise, again, rather than an
  error, we will get the wrong output.^[In fact, while the
  implementation of `matProd` breezes past GHC it is quite
  wrong!]

Specifying List Dimensions
--------------------------

In order to start reasoning about dimensions, we need a way
to represent the *dimension* of a list inside the refinement
logic. ^[We could just use `vDim`, but that is a cheat as
there is no guarantee that the field's value actually equals
the size of the list!]

\newthought{Measures} are ideal for this
task. [Previously](#boolmeasures) we saw how we could lift
Haskell functions up to the refinement logic. Lets write a
measure to describe the length of a list: ^[[Recall](#usingmeasures)
that these must be inductively defined functions, with a single
equation per data-constructor]

\begin{code}
{-@ measure size @-}
{-@ size    :: xs:[a] -> {v:Nat | v = size xs} @-}
size []     = 0
size (_:rs) = 1 + size rs
\end{code}

\newthought{Measures Refine Constructors}
As with [refined data definitions](#autosmart), the
measures are translated into strengthened types for
the type's constructors. For example, the `size`
measure is translated into:

~~~~~{.spec}
data [a] where
  []  :: {v: [a] | size v = 0}
  (:) :: a -> xs:[a] -> {v:[a]|size v = 1 + size xs}
~~~~~

\newthought{Multiple Measures} may be defined for the same data
type. For example, in addition to the `size` measure, we can define a
`notEmpty` measure for the list type:

\begin{code}
{-@ measure notEmpty @-}
notEmpty       :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True
\end{code}


\newthought{We Compose Different Measures}
simply by *conjoining* the refinements in the strengthened
constructors. For example, the two measures for lists end
up yielding the constructors:

~~~~~{.spec}
data [a] where
  []  :: {v: [a] | not (notEmpty v) && size v = 0}
  (:) :: a
      -> xs:[a]
      -> {v:[a]| notEmpty v && size v = 1 + size xs}
~~~~~

\noindent
This is a very significant advantage of using measures
instead of indices as in [DML][dml] or [Agda][agdavec],
as *decouples property from structure*, which crucially
enables the use of the same structure for many different
purposes. That is, we need not know *a priori* what indices
to bake into the structure, but can define a generic
structure and refine it *a posteriori* as needed with
new measures.

We are almost ready to begin creating a dimension aware API
for lists; one last thing that is useful is a couple of aliases
for describing lists of a given dimension.

\newthought{To make signatures symmetric} lets define an alias for
plain old (unrefined) lists:

\begin{code}
type List a = [a]
\end{code}

<div class="toolinfo">
\newthought{A ListN} is a list with exactly `N` elements, and a
`ListX` is a list whose size is the same as another list `X`.  Note
that when defining refinement type aliases, we use uppercase variables
like `N` and `X` to distinguish *value* parameters from the lowercase
*type* parameters like `a`.
</div>

\begin{code}
{-@ type ListN a N = {v:List a | size v = N} @-}
{-@ type ListX a X = ListN a {size X}        @-}
\end{code}


Lists: Size Preserving API
--------------------------

With the types and aliases firmly in our pockets, let us
write dimension-aware variants of the usual list functions.
The implementations are the same as in the standard library
i.e. [`Data.List`][data-list], but the specifications are
enriched with dimension information.

<div class="hwex" id="Map">
\newthought{map} yields a list with the same size as the input.
Fix the specification of `map` so that the `prop_map` is verified.
</div>

\begin{code}
{-@ map      :: (a -> b) -> xs:List a -> List b @-}
map _ []     = []
map f (x:xs) = f x : map f xs

{-@ prop_map :: List a -> TRUE @-}
prop_map xs = size ys == size xs
  where
    ys      = map id xs
\end{code}

<div class="hwex" id="Reverse"> \singlestar
We can `reverse` the elements of a list as shown below, using the
tail recursive function `go`. Fix the signature for `go`
so that LiquidHaskell can prove the specification for `reverse`.
</div>

\hint How big is the list returned by `go`?

\begin{code}
{-@ reverse       :: xs:List a -> ListX a xs @-}
reverse xs        = go [] xs
  where
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs
\end{code}


\newthought{zipWith} requires both lists to have the *same* size,
and produces a list with that same size. ^[As made explicit by
the call to `die`, the input type *rules out* the case where one
list is empty and the other is not, as in that case the former's
length is zero while the latter's is not, and hence, different.]

\begin{code}
{-@ zipWith :: (a -> b -> c) -> xs:List a
                             -> ListX b xs
                             -> ListX c xs
  @-}
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ [] []         = []
zipWith _ _  _          = die "no other cases"
\end{code}

\newthought{unsafeZip} The signature for `zipWith` is quite severe -- it
rules out the case where the zipping occurs only upto the shorter input.
Here's a function that actually allows for that case, where the output
type is the *shorter* of the two inputs:

\begin{code}
{-@ zip :: as:[a] -> bs:[b] -> {v:[(a,b)] | Tinier v as bs} @-}
zip (a:as) (b:bs) = (a, b) : zip as bs
zip [] _          = []
zip _  []         = []
\end{code}

\noindent The output type uses the predicate `Tinier Xs Ys Zs`
which defines the length of `Xs` to be the smaller of that of
`Ys` and `Zs`.^[In logic, `if p then q else r` is the same as
`p => q && not p => r`.]

\begin{code}
{-@ predicate Tinier X Y Z = Min (size X) (size Y) (size Z) @-}
{-@ predicate Min X Y Z = (if Y < Z then X = Y else X = Z)  @-}
\end{code}




<div class="hwex" id="Zip Unless Empty"> \doublestar
In my experience, `zip` as shown above is far too
permissive and lets all sorts of bugs into my code. As middle
ground, consider `zipOrNull` below. Write a specification
for `zipOrNull` such that the code below is verified by
LiquidHaskell.
</div>

\begin{code}
zipOrNull       :: [a] -> [b] -> [(a, b)]
zipOrNull [] _  = []
zipOrNull _ []  = []
zipOrNull xs ys = zipWith (,) xs ys

{-@ test1 :: {v: _ | size v = 2} @-}
test1     = zipOrNull [0, 1] [True, False]

{-@ test2 :: {v: _ | size v = 0} @-}
test2     = zipOrNull [] [True, False]

{-@ test3 :: {v: _ | size v = 0} @-}
test3     = zipOrNull ["cat", "dog"] []
\end{code}

\hint Yes, the type is rather gross; it uses a bunch of
      disjunctions `||` , conjunctions `&&` and implications `=>`.

Lists: Size Reducing API {#listreducing}
------------------------

Next, lets look at some functions that truncate lists, in one way or another.

\newthought{Take} lets us grab the first `k` elements from a list:

\begin{code}
{-@ take'     :: n:Nat -> ListGE a n -> ListN a n @-}
take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs
take' _ _      = die "won't  happen"
\end{code}

\noindent The alias `ListGE a n` denotes lists whose
length is at least `n`:

\begin{code}
{-@ type ListGE a N = {v:List a | N <= size v} @-}
\end{code}

<div class="hwex" id="Drop">
`Drop` is the yang to `take`'s yin: it returns the remainder after extracting
the first `k` elements. Write a suitable specification for it so that the below
typechecks.
</div>

\begin{code}
drop 0 xs     = xs
drop n (_:xs) = drop (n-1) xs
drop _ _      = die "won't happen"

{-@ test4 :: ListN String 2 @-}
test4 = drop 1 ["cat", "dog", "mouse"]
\end{code}

<div class="hwex" id="Take it easy">
The version `take'` above is too restrictive;
it insists that the list actually have at least `n` elements.
Modify the signature for the *real* `take` function so that
the code below is accepted by LiquidHaskell.
</div>

\begin{code}
take 0 _       = []
take _ []      = []
take n (x:xs)  = x : take (n-1) xs

{-@ test5 :: [ListN String 2] @-}
test5 = [ take 2  ["cat", "dog", "mouse"]
        , take 20 ["cow", "goat"]        ]
\end{code}

\newthought{The Partition} function breaks a list into two
sub-lists of elements that either satisfy or fail a user
supplied predicate.

\begin{code}
partition          :: (a -> Bool) -> [a] -> ([a], [a])
partition _ []     = ([], [])
partition f (x:xs)
  | f x            = (x:ys, zs)
  | otherwise      = (ys, x:zs)
  where
    (ys, zs)       = partition f xs
\end{code}

We would like to specify that the *sum* of the output tuple's
dimensions equal the input list's dimension. Lets write measures
to access the elements of the output:

~~~~~{.spec}
{-@ measure fst @-}
fst  (x, _) = x

{-@ measure snd @-}
snd (_, y) = y
~~~~~

\noindent We can now refine the type of `partition` as:

\begin{code}
{-@ partition :: _ -> xs:_ -> {v:_ | Sum2 v (size xs)} @-}
\end{code}

\noindent where `Sum2 V N` holds for a pair of lists dimensions add to `N`:

\begin{code}
{-@ predicate Sum2 X N = size (fst X) + size (snd X) = N @-}
\end{code}

<div class="hwex" id="QuickSort">
Use `partition` to implement `quickSort`.
</div>

\begin{code}
-- >> quickSort [1,4,3,2]
-- [1,2,3,4]

{-@ quickSort    :: (Ord a) => xs:List a -> ListX a xs @-}
quickSort []     = []
quickSort (x:xs) = undefined

{-@ test10 :: ListN String 2 @-}
test10 = quickSort test4
\end{code}


Dimension Safe Vector API
-------------------------

We can use the dimension aware lists to create a safe vector API.

\newthought{Legal Vectors} are those whose `vDim` field actually
 equals the size of the underlying list:

\begin{code}
{-@ data Vector a = V { vDim  :: Nat
                      , vElts :: ListN a vDim }         @-}
\end{code}

When `vDim` is used a selector function, it returns the `vDim` field of `x`.

\begin{code}
{-@ vDim :: x:_ -> {v: Nat | v = vDim x} @-}
\end{code}

\noindent
The refined data type prevents the creation of illegal vectors:

\begin{code}
okVec  = V 2 [10, 20]       -- accepted by LH

badVec = V 2 [10, 20, 30]   -- rejected by LH
\end{code}

\noindent
As usual, it will be handy to have a few aliases.

\begin{code}
-- | Non Empty Vectors
{-@ type VectorNE a  = {v:Vector a | vDim v > 0} @-}

-- | Vectors of size N
{-@ type VectorN a N = {v:Vector a | vDim v = N} @-}

-- | Vectors of Size Equal to Another Vector X
{-@ type VectorX a X = VectorN a {vDim X}        @-}
\end{code}

\newthought{To Create} a `Vector` safely, we can
start with the empty vector `vEmp` and then add
elements one-by-one with `vCons`:

\begin{code}
{-@ vEmp :: VectorN a 0 @-}
vEmp = V 0 []

{-@ vCons :: a -> x:Vector a -> VectorN a {vDim x + 1} @-}
vCons x (V n xs) = V (n+1) (x:xs)
\end{code}

\newthought{To Access} vectors at a low-level, we can use
equivalents of *head* and *tail*, which only work on
non-empty `Vector`s:

\begin{code}
{-@ vHd :: VectorNE a -> a @-}
vHd (V _ (x:_))  = x
vHd _            = die "nope"

{-@ vTl          :: x:VectorNE a -> VectorN a {vDim x - 1} @-}
vTl (V n (_:xs)) = V (n-1) xs
vTl _            = die "nope"
\end{code}

\newthought{To Iterate} over a vector we can use the `for` combinator:

\begin{code}
{-@ for        :: x:Vector a -> (a -> b) -> VectorX b x @-}
for (V n xs) f = V n (map f xs)
\end{code}



\newthought{Binary Pointwise Operations} should only be applied
to *compatible* vectors, i.e. vectors with equal dimensions.
We can write a generic binary pointwise operator:

\begin{code}
{-@ vBin :: (a -> b -> c) -> x:Vector a
                          -> VectorX b x
                          -> VectorX c x
  @-}
vBin op (V n xs) (V _ ys) = V n (zipWith op xs ys)
\end{code}

\newthought{The Dot Product} of two `Vector`s can be now implemented
in a wholemeal *and* dimension safe manner, as:

\begin{code}
{-@ dotProduct :: (Num a) => x:Vector a -> VectorX a x -> a @-}
dotProduct x y = sum $ vElts $ vBin (*) x y
\end{code}

<div class="hwex" id="Vector Constructor">
Complete the *specification* and *implementation* of `vecFromList`
which *creates* a `Vector` from a plain list.
</div>

\begin{code}
vecFromList     :: [a] -> Vector a
vecFromList xs  =  undefined

test6  = dotProduct vx vy    -- should be accepted by LH
  where
    vx = vecFromList [1,2,3]
    vy = vecFromList [4,5,6]
\end{code}

<div class="hwex" id="Flatten">
\singlestar Write a function to `flatten` a nested `Vector`.
</div>

\begin{code}
{-@ flatten :: n:Nat
            -> m:Nat
            -> VectorN (VectorN a m) n
            -> VectorN a {m * n}
  @-}
flatten = undefined
\end{code}

\newthought{The Cross Product} of two vectors can now be
computed in a nice wholemeal style, by a nested iteration
followed by a `flatten`.

\begin{code}
{-@ product   :: xs:Vector _
              -> ys:Vector _
              -> VectorN _ {vDim xs * vDim ys}
  @-}
product xs ys = flatten (vDim ys) (vDim xs) xys
  where
    xys       = for ys $ \y ->
                  for xs $ \x ->
                    x * y
\end{code}


Dimension Safe Matrix API
-------------------------

The same methods let us create a dimension safe Matrix API which
ensures that only legal matrices are created and that operations
are performed on compatible matrices.

\newthought{Legal Matrices} are those where the dimension of the
outer vector equals the number of rows `mRow` and the dimension
of each inner vector is `mCol`. We can specify legality in a
refined data definition:

\begin{code}
{-@ data Matrix a =
      M { mRow  :: Pos
        , mCol  :: Pos
        , mElts :: VectorN (VectorN a mCol) mRow
        }
  @-}
\end{code}

\noindent Notice that we avoid disallow degenerate matrices by
requiring the dimensions to be positive.

\begin{code}
{-@ type Pos = {v:Int | 0 < v} @-}
\end{code}

\noindent It is convenient to have an alias for matrices of a given size:

\begin{code}
{-@ type MatrixN a R C   = {v:Matrix a | Dims v R C } @-}
{-@ predicate Dims M R C = mRow M = R && mCol M = C   @-}
\end{code}

\noindent For example, we can use the above to write type:

\begin{code}
{-@ ok23 :: MatrixN _ 2 3 @-}
ok23     = M 2 3 (V 2 [ V 3 [1, 2, 3]
                      , V 3 [4, 5, 6] ])
\end{code}

<div class="hwex" id="Legal Matrix">
Modify the definitions of `bad1` and `bad2`
so that they are legal matrices accepted by
LiquidHaskell.
</div>

\begin{code}
bad1 :: Matrix Int
bad1 = M 2 3 (V 2 [ V 3 [1, 2   ]
                  , V 3 [4, 5, 6]])

bad2 :: Matrix Int
bad2 = M 2 3 (V 2 [ V 2 [1, 2]
                  , V 2 [4, 5] ])
\end{code}

<div class="hwex" id="Matrix Constructor">
\singlestar Write a function to construct
a `Matrix` from a nested list.
</div>

\begin{code}
matFromList      :: [[a]] -> Maybe (Matrix a)
matFromList []   = Nothing
matFromList xss@(xs:_)
  | ok           = Just (M r c vs)
  | otherwise    = Nothing
  where
    r            = size xss
    c            = size xs
    ok           = undefined
    vs           = undefined
\end{code}

<div class="hwex" id="Refined Matrix Constructor">
\doublestar Refine the specification for `matFromList`
so that the following is accepted by LiquidHaskell.
</div>

\begin{code}
{-@ mat23 :: Maybe (MatrixN Integer 2 2) @-}
mat23     = matFromList [ [1, 2]
                        , [3, 4] ]
\end{code}

\hint It is easy to specify the number of rows from `xss`.
How will you figure out the number of columns? A measure
may be useful.

\newthought{Matrix Multiplication} Finally, lets implement matrix
multiplication. You'd think we did it already, but in fact the
implementation at the top of this chapter is all wrong (run it and
see!) We cannot just multiply any two matrices: the number of
*columns* of the first must equal to the *rows* of the second -- after
which point the result comprises the `dotProduct` of the rows of the
first matrix with the columns of the second.

\begin{code}
{-@ matProduct :: (Num a) => x:Matrix a
                          -> y:{Matrix a  | mCol x = mRow y}
                          -> MatrixN a (mRow x) (mCol y)
  @-}
matProduct (M rx _ xs) my@(M _ cy _)
                 = M rx cy elts
  where
    elts         = for xs $ \xi ->
                     for ys' $ \yj ->
                       dotProduct xi yj
    M _ _ ys'    = transpose my
\end{code}

\noindent To iterate over the *columns* of the matrix
`my` we just `transpose` it so the columns become rows.

\begin{code}
-- >>> ok32 == transpose ok23
-- True
ok32 = M 3 2 (V 3 [ V 2 [1, 4]
                  , V 2 [2, 5]
                  , V 2 [3, 6] ])
\end{code}

<div class="hwex" id="Matrix Transpose">
\doublestar Use the `Vector` API to complete the implementation
of `txgo`. For inspiration, you might look at the implementation
of `Data.List.transpose` from the [prelude][URL-transpose].
Better still, don't.
</div>

\begin{code}
{-@ transpose :: m:Matrix a -> MatrixN a (mCol m) (mRow m) @-}
transpose (M r c rows) = M c r $ txgo c r rows

{-@ txgo      :: c:Nat -> r:Nat
              -> VectorN (VectorN a c) r
              -> VectorN (VectorN a r) c
  @-}
txgo c r rows = undefined
\end{code}

\hint As shown by `ok23` and `ok32`, `transpose` works by
stripping out the `head`s of the input rows, to create the
corresponding output rows.


Recap
-----

In this chapter, we saw how to use measures to describe
numeric properties of structures like lists (`Vector`)
and nested lists (`Matrix`).

1. Measures are *structurally recursive* functions, with a single
   equation per data constructor,

2. Measures can be used to create refined data definitions
   that prevent the creation of illegal values,

3. Measures can then be used to enable safe wholemeal programming,
   via dimension-aware APIs that ensure that operators only apply to
   compatible values.

We can use numeric measures to encode various other
properties of data structures. We will see examples
ranging from high-level [AVL trees](#case-study-avltree),
to low-level safe [pointer arithmetic](#case-study-pointers).
