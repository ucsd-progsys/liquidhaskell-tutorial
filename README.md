# LiquidHaskell Tutorial

## Contents

### Part I: Refinement Types

0. Install

1. Refinement Types

2. Polymorphism & HOFs

3. Refining Datatypes

### Part II: Measures

4. Propositions

5. Numbers

6. Sets

### Part III : Case Studies

7. Case Study: Lazy Queue

8. Case Study: Associative Maps

9. Case Study: Pointers without Overflows

10. Case Study: AVL Trees

## TODO

### Part IV : Abstract Refinements (TODO) 

10. Abstract Refinements I (code)
  + Copy from FLOPS/IHP talk sequence
  + Vanilla/Code [compose, foldr, ...]

11. Abstract Refinements II (data)
  + RecRef [List, BST]
  + Arrays

12. Abstract Refinements III (constraints)
  + compose
  + filter
  + state 

### Part V: Tips and Tricks (TODO)

13. Tips:
     + Inductive strengthening 
     + Materializing Proofs
     + Assumes/Dynamic Checking
     
15. Tricks: Totality

16. Tricks: Termination
  + Copy from BLOG/PAPER sequence
  + HW Exercises

### Extra Case Studies

+ Case Study 1: AlphaConvert (tests/pos/alphaconvert-List.hs) 
+ Case Study 2: Kmeans


## TODO 

- grep FIXME/TODO (!)

- Subtyping exercises
    - div by zero
    - array-bounds
    - create (bytestring)

- LH fix
  - allow using CoreToLogic definitions (e.g. member) in
    predicates/aliases not just other measures #332
  
- convert measure refinements into invariants, e.g.

  measure size :: [Int] -> Nat

  should yield invariant {v:[a] | 0 <= size v}

+ Printing of predicates (we drop required parens)

? Intelligible parse errors

+ Web demo 

Chapter 4
----------

+ align source names and chapter names (00 --> 01)
+ too early to do BOTH spec and implementation (one at a time)
+


Gotchas
------- 

- hs sig vs. lh sig
- module and import story
- `inline`

- Binders: If I enter the following specification:
{-@ plus :: v1:Sparse a
         -> {v2:Sparse a | spDim v1 == spDim v2}
         -> {v3:Sparse a | spDim v3 == spDim v2 && spDim v3 == spDim v1}
     @-}

LH tells me that v2 is unbound. I feel that it should not be given that I defined v2 in the second argument to the spec.

* Ch4 refined datatypes p35
  All types should derive Show so that a reader following along can try functions in GHCI and get a result printed

* Pattern matching gives you invariants but raw field access doesn't. See Chris's note on 4.6
4.6)I came up with:

del :: (Ord a) => a -> BST a -> BST a

del k' t@(Node k l r)

 | k' < k = Node k (del k' l) r

 | k' > k = Node k l (del k' r)

 | otherwise = remv t

 where

 remv :: (Ord a) => BST a -> BST a
 remv (Node _ Leaf Leaf) = Leaf
 remv (Node _ l Leaf) = l
 remv (Node _ Leaf r) = r
 remv (Node _ l r) = Node iSuccE l iSuccR
 where
   iSuccE = mElt $ delMin r
   iSuccR = rest $ delMin r

However, LH complains about iSuccR. I don't
see why it should have a problem with that
given that iSuccR is the rest of the right
tree which is by definition greater than
iSuccE, which is the smallest element of the
right tree. I imagine I could walk the entire
right tree to verify this to please LH like I
did with quickSort, but I can't imagine that
is the correct answer.


 - in addition to the usual syntax:

     {-@ unsafeLookup :: n:Nat ->  {v:Vector a | n < (vlen v)} -> a @-}

     - could we get a "function" syntax:

     {-@ unsafeLookup :: n:Nat ->  v:Vector a -> a
         unsafeLookup n v = n < vlen v @-}

         ...or...

     {-@ unsafeLookup :: n:Int -> v:Vector a ->
         unsafeLookup n v = n >= 0 && n < vlen v @-}

         ...or...

     {-@ unsafeLookup :: n:Int -> {v:Vector a | n < (vlen v)} -> a
         unsafeLookup n v = n >= 0 @-}

     - the "function" could take as arguments symbols that were bound
       in the "signature." It could take a partial specification and
       implement additional refinements. It "looks like Haskell" and
       allows the programmer more freedom in how they define their
       refinements
