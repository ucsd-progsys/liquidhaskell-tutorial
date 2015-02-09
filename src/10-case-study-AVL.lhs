Case Study: AVL Trees {#case-study-avltree}
================================


<div class="hidden">
\begin{code}
{- Example of AVL trees by michaelbeaumont -}

{-@ LIQUID "--diff"           @-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--totality"       @-}

module AVL (AVL, empty, singleton, insert, insert', delete) where

import Prelude hiding (max)
-- import Language.Haskell.Liquid.Prelude (liquidAssume)

-- Test
main = do
    mapM_ print [a, b, c, d]
  where
    a = singleton 5 
    b = insert 2 a
    c = insert 3 b
    d = insert 7 c

-- | Height is actual height (will disappear with measure-generated-invariants) ------------

{-@ invariant {v:AVL a | 0 <= realHeight v && realHeight v = getHeight v} @-}

{-@ inv_proof  :: t:AVL a -> {v:_ | 0 <= realHeight t && realHeight t = getHeight t } @-}
inv_proof Leaf           = True 
inv_proof (Node k l r n) = inv_proof l && inv_proof r

{-@ node :: x:a -> l:AVLL a x -> r:{AVLR a x | isBal l r 1} -> {v:AVL a | realHeight v = nodeHeight l r} @-}
node v l r = Node v l r (nodeHeight l r)
\end{code}
</div>


*This chapter is based on code by Michael Beaumont.*


One of the most fundamental abstractions in computing, is that of a
*collection* of values -- names, numbers, records -- into which we can
rapidly `insert`, `delete` and check for `member`ship.

\newthought{Trees} offer an an attractive means of implementing
collections in the immutable setting. We can *order* the values
to ensure that each operation takes time proportional to the
*path* from the root to the datum being operated upon.
If we additionally keep the tree *balanced* then each path
is small (relative to the size of the collection), thereby
giving us an efficient implementation for collections.

\newthought{As in real life}
maintaining order and balance is rather easier said than done.
Often we must go through rather sophisticated gymnastics to ensure
everything is in its right place. Fortunately, LiquidHaskell can help.
Lets see a concrete example, that should be familiar from your introductory
data structures class: the Georgy Adelson-Velsky and Landis' or [AVL Tree][avl-wiki].

AVL Trees 
---------

An `AVL` tree is defined by the following Haskell datatype:

\begin{code}
data AVL a =
    Leaf
  | Node { key :: a      -- value
         , l   :: AVL a  -- left subtree
         , r   :: AVL a  -- right subtree
         , ah  :: Int    -- height
         }
    deriving (Show)
\end{code}

While the Haskell type signature describes any old binary tree, an
`AVL` tree like that shown in Figure~\ref{fig:avl} actually satisfies
two crucial invariants: it should be binary search ordered and
balanced.


\begin{marginfigure}[h]
\includegraphics[height=1.5in]{img/avl.png}
\caption{An AVL tree is an ordered, height-balanced tree.}
\label{fig:avl}
\end{marginfigure}

\newthought{A Binary Search Ordered} tree is one where at *each*
`Node`, the values of the `left` and `right` subtrees are strictly
less and greater than the values at the `Node`. For example, in the
tree in Figure~\ref{fig:avl} the root has value `50` while its left
and right subtrees have values in the range `9-23` and `54-76`
respectively.  This holds at all nodes, not just the root. For
example, the node `12` has left and right children strictly less and
greater than `12`.

\newthought{A Balanced} tree is one where at *each* node, the *heights*
of the left and right subtrees differ by at most `1`. For example, in
Figure~\ref{fig:avl}, at the root, the heights of the left and right subtrees
are the same, but at the node `72` the left subtree has height `2` which is
one more then the right subtree.

Order ensures that there is at most a single path of `left` and
`right` moves from the root at which an element can be found; balance
ensures that each such path in the tree is of size $O(\log\ n)$ where
$n$ is the numbers of nodes. Thus, together they ensure that the
collection operations are efficient: they take time logarithmic in the
size of the collection.

Specifying AVL Trees
--------------------

The tricky bit is to ensure order and balance. Before we can ensure
anything, lets tell LiquidHaskell what we mean by these terms, by
defining legal or valid AVL trees.

\newthought{To Specify Order} we just define two aliases `AVLL` and `AVLR`
-- read *AVL-left* and *AVL-right* for trees whose values are strictly less
than and greater than some value `X`:

\begin{code}
-- | Trees with value less than X
{-@ type AVLL a X = AVL {v:a | v < X}  @-}

-- | Trees with value greater than X
{-@ type AVLR a X = AVL {v:a | X < v}  @-}
\end{code}


\newthought{The Real Height} of a tree is defined recursively as `0`
for `Leaf`s and one more than the larger of left and right subtrees
for `Node`s.  Note that we cannot simply use the `ah` field because
thats just some arbitrary `Int` -- there is nothing to prevent a buggy
implementation from just filling that field with `0` everywhere.  In
short, we need the ground truth: a measure that computes the *actual*
height of a tree.

\begin{code}
{-@ measure realHeight @-}
realHeight                :: AVL a -> Int
realHeight Leaf           = 0
realHeight (Node _ l r _) = nodeHeight l r

{-@ inline nodeHeight @-}
nodeHeight l r = 1 + max hl hr
  where
    hl             = realHeight l
    hr             = realHeight r                            

{-@ inline max @-}
max :: Int -> Int -> Int
max x y = if x > y then x else y
\end{code}

<div class="toolinfo">
The `inline` pragmas indicate that the Haskell
functions can be directly lifted into and used inside the
refinement logic and measures.
</div>

\noindent
We can now say that a value `v` is indeed the *real* height of a
node with subtrees `l` and `r` if the predicate `isReal v l r` holds:

\begin{code}
{-@ inline isReal @-}
isReal v l r = v == nodeHeight l r
\end{code} 

\newthought{A Node is $n$-Balanced} if its left and right subtrees
have a (real) height difference of at most $n$. We can specify this
requirement as a predicate `isBal l r n`

\begin{code}
{-@ inline isBal @-}
isBal l r n = 0 - n  <= d && d <= n
  where
    d       = hl - hr 
    hl      = realHeight l
    hr      = realHeight r
\end{code}

\newthought{A Legal AVL Tree} can now be defined via the following
[refined data type](#refineddatatypes), which states that each `Node`
is $1$-balanced, and that the saved height field is indeed the *real* height:

\begin{code}
{-@ data AVL a =
        Leaf
      | Node { key :: a
             , l   :: AVLL a key
             , r   :: {v:AVLR a key | isBal l v 1} 
             , ah  :: {v:Nat        | isReal v l r}
             }
  @-}
\end{code}

Smart Constructors
------------------

Lets use the type to construct a few small trees which will
also be handy in a general collection API. First, lets write
an alias for trees of a given height:
 
\begin{code}
-- | Trees of height N
{-@ type AVLN a N = {v: AVL a | realHeight v = N} @-}

-- | Trees of height equal to that of another T
{-@ type AVLT a T = AVLN a {realHeight T} @-}
\end{code}
 
\newthought{An Empty} collection is represented by a `Leaf`,
which has height `0`:

\begin{code}
{-@ empty :: AVLN a 0 @-}
empty = Leaf
\end{code}

<div class="hwex" id="Singleton">
Consider the function `singleton` that builds an `AVL`
tree from a single element. Fix the code below so that
it is accepted by LiquidHaskell.

\begin{code}
{-@ singleton :: a -> AVLN a 1 @-}
singleton x =  Node x empty empty 0 
\end{code}

As you can imagine, it can be quite tedious to keep the saved height
field `ah` *in sync* with the *real* height. In general in such
situations, which arose also with [lazy queues](#lazyqueue), the right
move is to eschew the data constructor and instead use a *smart
constructor* that will fill in the appropriate values correctly.

<div class="footnotetext">By the way, you might wonder: why do we
bother to save the height anyway? Why not just recompute it instead? 
</div>

\newthought{The Smart Constructor} `node` takes as input the node's value `x`,
left and right subtrees `l` and `r` and returns a tree by filling in the right
value for the height field. 

\begin{code}
{-@ mkNode :: a -> l:AVL a -> r:AVL a -> AVLN a {nodeHeight l r} @-}
mkNode v l r = Node v l r h
 where
   h       = 1 + max hl hr
   hl      = getHeight l
   hr      = getHeight r
\end{code}

<div class="hwex" id="Constructor">Unfortunately, LiquidHaskell  rejects
the above smart constructor `node`. Can you explain why? Can you fix the 
code (implementation or specification) so that the function is accepted?
</div>

\hint Think about the (refined) type of the actual constructor `Node`, and
the properties it requires and ensures. 


Inserting Elements
------------------

Next, lets turn our attention to the problem of *adding* elements to 
an `AVL` tree. The basic strategy is this:

1. *Find* the appropriate location in the tree to add the value, 
   using binary search ordering
2. *Replace* the `Leaf` at that location with the singleton 
   containing the value.

\noindent If you prefer the spare precision of Haskell to the 
informality of English, here is a first stab at implementing 
the insert function:

\begin{code}
{-@ insert0    :: (Ord a) => a -> AVL a -> AVL a @-}
insert0 y t@(Node x l r _) 
  | y < x      =  node x (insert0 y l) r
  | x < y      =  node x l (insert0 y r)
  | otherwise  = t 
insert0 y Leaf = singleton y
\end{code}

\newthought{Unfortunately} `insert0` does not work. 
Here `node` is a fixed variant of the smart constructor `mkNode`;
If you did the exercise above, you can replace it with `mkNode` and 
you will see that the above function is rejected by LiquidHaskell.
The error message would essentially say that at the calls to the
smart constructor, the arguments violate the balance requirement.

\newthought{To see why} take a look at Figure~\ref{fig:avl-insert-naive}.
The new value `y` is inserted into the left subtree, which is already 
bigger than the right by a factor of `1`.  The insert increases the 
height difference further to `2` making it impossible to (directly) 
link up the `insert0 y l` and `r` using `node`.

  
Refactoring Balance
-------------------

Deleting Elements  
-----------------

Functional Correctness
----------------------

\begin{code}
{-@ predicate NoHeavy    T = bFac T == 0                      @-}
{-@ predicate LeftHeavy  T = bFac T == 1                      @-}
{-@ predicate RightHeavy T = bFac T == -1                     @-}

{-@ inline htDiff @-}
htDiff s t d = (realHeight s - realHeight t) == d

{-@ inline eq1 @-}
eq1 s t      = htDiff t s 0 || htDiff t s 1

--------------------------------------------------------------------------------------------
-- | Constructor & Destructor  -------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- | Compute Tree Height -------------------------------------------------------------------
    
{-@ measure getHeight @-}
{-@ getHeight            :: t:_ -> {v:Nat | v = realHeight t} @-}
getHeight Leaf           = 0
getHeight (Node _ _ _ n) = n


-- | Compute Balance Factor ----------------------------------------------------------------

{-@ measure bFac @-}
{-@ bFac :: t:AVL a -> {v:Int | v = bFac t && 0 <= v + 1 && v <= 1} @-}
bFac Leaf           = 0
bFac (Node _ l r _) = getHeight l - getHeight r 

--------------------------------------------------------------------------------------------
-- | API: Insert 1 (Beaumont) -------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ insert :: a -> s:AVL a -> {t: AVL a | eq1 s t} @-}
insert a Leaf             = singleton a
insert a t@(Node v _ _ _) = case compare a v of
                              LT -> insL a t 
                              GT -> insR a t
                              EQ -> t

{-@ insL :: x:a -> s:{AVL a | x < key s && realHeight s > 0} -> {t: AVL a | eq1 s t} @-}
insL a (Node v l r _)
  | leftBig && bl' > 0 = balLL v l' r
  | leftBig && bl' < 0 = balLR v l' r
  | leftBig            = balL0 v l' r
  | otherwise          = node v l' r
  where
    leftBig            = getHeight l' - getHeight r > 1
    l'                 = insert a l
    bl'                = bFac l'

-- Skip this
    
{-@ insR :: x:a -> s:{AVL a | key s < x && realHeight s > 0} -> {t: AVL a | eq1 s t} @-}
insR a (Node v l r _)
  | rightBig && br' > 0  = balRL v l r'
  | rightBig && br' < 0  = balRR v l r'
  | rightBig             = balR0 v l r'
  | otherwise            = node v l r'
  where
    rightBig             = getHeight r' - getHeight l > 1
    r'                   = insert a r
    br'                  = bFac r'

--------------------------------------------------------------------------------------------
-- | API: Insert 2 (Leroy) ----------------------------------------------------------------- 
--------------------------------------------------------------------------------------------


{-@ insert' :: a -> s:AVL a -> {t: AVL a | eq1 s t} @-}
insert' a Leaf             = singleton a
insert' a t@(Node v l r n) = case compare a v of
                               LT -> bal v (insert' a l) r 
                               GT -> bal v l (insert' a r) 
                               EQ -> t



--------------------------------------------------------------------------------------------
-- | API: Delete --------------------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ delete               :: a -> s:AVL a -> {t: AVL a | eq1 t s} @-}
delete _ Leaf            = Leaf
delete a (Node v l r _)  = case compare a v of
                            LT -> bal v (delete a l) r 
                            GT -> bal v l (delete a r) 
                            EQ -> merge v l r


merge :: a -> AVL a -> AVL a -> AVL a
merge _ Leaf t           =  t
merge _ t Leaf           =  t
merge v t1 t2            =  bal a t1 t2'
  where
    (a, t2')             =  getMin t2
                        
getMin (Node x Leaf r _) = (x, r)
getMin (Node x l r _)    = (x', bal x l' r)
  where
    (x', l')             = getMin l



--------------------------------------------------------------------------------------------
-- | Generalized Balancing  ---------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ predicate RBal L R T   = if (realHeight L >= realHeight R) then (eq1 L T) else eq1 R T @-}

{-@ bal :: x:a
        -> l:AVLL a x
        -> r:{AVLR a x | isBal l r 2}
        -> {t: AVL a | (isBal l r 1 => isReal (realHeight t) l r) && RBal l r t}
  @-}
bal v l r
  | leftBig  && bl > 0 = balLL v l r
  | leftBig  && bl < 0 = balLR v l r
  | leftBig            = balL0 v l r
  | rightBig && br > 0 = balRL v l r
  | rightBig && br < 0 = balRR v l r
  | rightBig           = balR0 v l r
  | otherwise          = node  v l r
  where
    leftBig            = siblDiff     > 1
    rightBig           = siblDiff + 1 < 0
    siblDiff           = getHeight l - getHeight r
    bl                 = bFac l
    br                 = bFac r

{-@ balL0 :: x:a -> l:{AVLL a x | NoHeavy l} -> r:{AVLR a x | htDiff l r 2} -> AVLN a {realHeight l + 1 } @-}
balL0 v (Node lv ll lr _) r
  = node lv ll (node v lr r)

{-@ balLL :: x:a -> l:{AVLL a x | LeftHeavy l } -> r:{AVLR a x | htDiff l r 2} -> AVLT a l @-}
balLL v (Node lv ll lr _) r
  = node lv ll (node v lr r)

{-@ balLR :: x:a -> l:{AVLL a x | RightHeavy l } -> r:{AVLR a x | htDiff l r 2} -> AVLT a l @-}
balLR v (Node lv ll (Node lrv lrl lrr _) _) r
  = node lrv (node lv ll lrl) (node v lrr r)

{-@ balR0 :: x:a -> l: AVLL a x -> r: {AVLR a x | NoHeavy r && htDiff r l 2 } -> AVLN a {realHeight r + 1} @-}
balR0 v l (Node rv rl rr _)
  = node rv (node v l rl) rr

{-@ balRR :: x:a -> l: AVLL a x -> r:{AVLR a x | RightHeavy r && htDiff r l 2 } -> AVLT a r @-}
balRR v l (Node rv rl rr _)
  = node rv (node v l rl) rr

{-@ balRL :: x:a -> l: AVLL a x -> r:{AVLR a x | LeftHeavy r && htDiff r l 2} -> AVLT a r @-}
balRL v l (Node rv (Node rlv rll rlr _) rr _)
  = node rlv (node v l rll) (node rv rlr rr) 
                                                      
\end{code}
