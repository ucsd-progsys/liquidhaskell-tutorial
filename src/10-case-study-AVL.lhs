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

\newthought{To Specify Balance} we need a way to talk about the *real height*
of the tree. We can't just reach for the `ah` field because thats just some
arbitrary `Int` -- there is nothing to prevent a buggy implementation from just
filling that field with `0` everywhere. In short, we need a reality check: a
measure that computes the real height of a tree.

\begin{code}
{-@ measure realHeight @-}
realHeight                :: AVL a -> Int
realHeight Leaf           = 0
realHeight (Node _ l r _) = 1 + max (realHeight l) (realHeight r)
\end{code}

<div class="toolinfo">
The function `max` just returns the larger of its two inputs. It can be lifted
into the refinement logic directly, as specified with the `inline` pragma 
</div>

\begin{code}
{-@ inline max @-}
max :: Int -> Int -> Int
max x y = if x > y then x else y
\end{code}


Smart Constructors
------------------

Inserting into a Tree
---------------------

Refactoring Balance
-------------------

Deleting from a Tree
-------------------- 

Functional Correctness
----------------------

\begin{code}
--------------------------------------------------------------------------------------------
-- | Datatype Definition ------------------------------------------------------------------- 
--------------------------------------------------------------------------------------------


{-@ data AVL a =
        Leaf
      | Node { key :: a
             , l   :: AVLL a key
             , r   :: AVLR a key 
             , ah  :: {v:Nat | HtBal l r 1 && v = nodeHeight l r}
             }
  @-}


-- | Predicate Aliases -------------------------------------------------------------------- 

{-@ predicate HtBal L R N  = 0 <= realHeight L - realHeight R + N && realHeight L - realHeight R <= N               @-}
{-@ predicate Ht N L R     = N = if (realHeight L) > (realHeight R) then (1 + realHeight L) else (1 + realHeight R) @-}
{-@ predicate HtDiff S T D = realHeight S - realHeight T == D                                       @-}
{-@ predicate EqHt S T     = HtDiff S T 0                                           @-}
{-@ predicate NoHeavy    T = bFac T == 0                                            @-}
{-@ predicate LeftHeavy  T = bFac T == 1                                            @-}
{-@ predicate RightHeavy T = bFac T == -1                                           @-}
{-@ predicate Eq1 S T      = EqHt T S || HtDiff T S 1                               @-}
{-@ predicate RBal L R T   = if (realHeight L >= realHeight R) then (Eq1 L T) else Eq1 R T          @-}

--------------------------------------------------------------------------------------------
-- | Constructor & Destructor  -------------------------------------------------------------
--------------------------------------------------------------------------------------------

-- | Smart Constructor (fills in height field) ---------------------------------------------

{-@ tree   :: x:a -> l:AVLL a x -> r:{AVLR a x | HtBal l r 1} -> {v:AVL a | realHeight v = nodeHeight l r} @-}
tree v l r = Node v l r (nodeHeight l r)


{-@ inline nodeHeight @-}
nodeHeight     :: AVL a -> AVL a -> Int
nodeHeight l r = 1 + max hl hr
  where
    hl         = getHeight l
    hr         = getHeight r


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

{-@ htDiff :: s:AVL a -> t: AVL a -> {v: Int | HtDiff s t v} @-}
htDiff     :: AVL a -> AVL a -> Int
htDiff l r = getHeight l - getHeight r


--------------------------------------------------------------------------------------------
-- | API: Empty ---------------------------------------------------------------------------- 
--------------------------------------------------------------------------------------------
             
{-@ empty :: {v: AVL a | realHeight v == 0} @-}
empty = Leaf

--------------------------------------------------------------------------------------------
-- | API: Singleton ------------------------------------------------------------------------ 
--------------------------------------------------------------------------------------------

{-@ singleton :: a -> {v: AVL a | realHeight v == 1 } @-}
singleton a = tree a empty empty 

--------------------------------------------------------------------------------------------
-- | API: Insert 1 (Beaumont) -------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ insert :: a -> s:AVL a -> {t: AVL a | Eq1 s t} @-}
insert a Leaf             = singleton a
insert a t@(Node v _ _ _) = case compare a v of
                              LT -> insL a t 
                              GT -> insR a t
                              EQ -> t

{-@ insL :: x:a -> s:{AVL a | x < key s && realHeight s > 0} -> {t: AVL a | Eq1 s t} @-}
insL a (Node v l r _)
  | leftBig && bl' > 0 = balLL v l' r
  | leftBig && bl' < 0 = balLR v l' r
  | leftBig            = balL0 v l' r
  | otherwise          = tree v l' r
  where
    leftBig            = siblDiff > 1
    siblDiff           = htDiff l' r
    l'                 = insert a l
    bl'                = bFac l'

-- Skip this
    
{-@ insR :: x:a -> s:{AVL a | key s < x && realHeight s > 0} -> {t: AVL a | Eq1 s t} @-}
insR a (Node v l r _)
  | rightBig && br' > 0  = balRL v l r'
  | rightBig && br' < 0  = balRR v l r'
  | rightBig             = balR0 v l r'
  | otherwise            = tree v l r'
  where
    rightBig             = siblDiff > 1
    siblDiff             = htDiff r' l
    r'                   = insert a r
    br'                  = bFac r'

--------------------------------------------------------------------------------------------
-- | API: Insert 2 (Leroy) ----------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ insert' :: a -> s:AVL a -> {t: AVL a | Eq1 s t} @-}
insert' a Leaf             = singleton a
insert' a t@(Node v l r n) = case compare a v of
                               LT -> bal v (insert' a l) r 
                               GT -> bal v l (insert' a r) 
                               EQ -> t



--------------------------------------------------------------------------------------------
-- | API: Delete --------------------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ delete               :: a -> s:AVL a -> {t: AVL a | Eq1 t s} @-}
delete _ Leaf            = Leaf
delete a (Node v l r _)  = case compare a v of
                            LT -> bal v (delete a l) r 
                            GT -> bal v l (delete a r) 
                            EQ -> merge v l r

merge                    :: a -> AVL a -> AVL a -> AVL a 
merge _ Leaf t           = t
merge _ t Leaf           = t
merge v t1 t2            = bal a t1 t2'
  where
    (a, t2')             = getMin t2
                        
getMin (Node x Leaf r _) = (x, r)
getMin (Node x l r _)    = (x', bal x l' r)
  where
    (x', l')             = getMin l

--------------------------------------------------------------------------------------------
-- | Generalized Balancing  ---------------------------------------------------------------- 
--------------------------------------------------------------------------------------------

{-@ bal :: x:a
        -> l:AVLL a x
        -> r:{AVLR a x | HtBal l r 2}
        -> {t: AVL a | (HtBal l r 1 => Ht (realHeight t) l r) && RBal l r t}
  @-}
bal v l r
  | leftBig  && bl > 0 = balLL v l r
  | leftBig  && bl < 0 = balLR v l r
  | leftBig            = balL0 v l r
  | rightBig && br > 0 = balRL v l r
  | rightBig && br < 0 = balRR v l r
  | rightBig           = balR0 v l r
  | otherwise          = tree  v l r
  where
    leftBig            = siblDiff     > 1
    rightBig           = siblDiff + 1 < 0
    siblDiff           = htDiff l r
    bl                 = bFac l
    br                 = bFac r

{-@ balL0 :: x:a -> l:{AVLL a x | NoHeavy l} -> r:{AVLR a x | HtDiff l r 2} -> {t:AVL a | realHeight t = realHeight l + 1 } @-}
balL0 v (Node lv ll lr _) r
  = tree lv ll (tree v lr r)

{-@ balLL :: x:a -> l:{AVLL a x | LeftHeavy l } -> r:{AVLR a x | HtDiff l r 2} -> {t:AVL a | EqHt t l} @-}
balLL v (Node lv ll lr _) r
  = tree lv ll (tree v lr r)

{-@ balLR :: x:a -> l:{AVLL a x | RightHeavy l } -> r:{AVLR a x | HtDiff l r 2} -> {t: AVL a | EqHt t l } @-}
balLR v (Node lv ll (Node lrv lrl lrr _) _) r
  = tree lrv (tree lv ll lrl) (tree v lrr r)

{-@ balR0 :: x:a -> l: AVLL a x -> r: {AVLR a x | NoHeavy r && HtDiff r l 2 } -> {t: AVL a | realHeight t = realHeight r + 1} @-}
balR0 v l (Node rv rl rr _)
  = tree rv (tree v l rl) rr

{-@ balRR :: x:a -> l: AVLL a x -> r:{AVLR a x | RightHeavy r && HtDiff r l 2 } -> {t: AVL a | EqHt t r } @-}
balRR v l (Node rv rl rr _)
  = tree rv (tree v l rl) rr

{-@ balRL :: x:a -> l: AVLL a x -> r:{AVLR a x | LeftHeavy r && HtDiff r l 2} -> {t: AVL a | EqHt t r } @-}
balRL v l (Node rv (Node rlv rll rlr _) rr _)
  = tree rlv (tree v l rll) (tree rv rlr rr) 
                                                      
\end{code}
