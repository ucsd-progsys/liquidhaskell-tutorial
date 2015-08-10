# Feedback and Gotchas

- hs sig vs. lh sig
- module and import story
- `inline`

- Binders: If I enter the following specification:
```
{-@ plus :: v1:Sparse a
         -> {v2:Sparse a | spDim v1 == spDim v2}
         -> {v3:Sparse a | spDim v3 == spDim v2 && spDim v3 == spDim v1}
     @-}
```
LH tells me that `v2` is unbound. I feel that it should not be given that I defined `v2` in the second argument to the spec.

* Ch4 refined datatypes p35

  All types should derive `Show` so that a reader following along can try functions in GHCI and get a result printed

* Pattern matching gives you invariants but raw field access doesn't. See Chris's note on 4.6
4.6)I came up with:
```
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
```
However, LH complains about `iSuccR`. I don't
see why it should have a problem with that
given that `iSuccR` is the rest of the right
tree which is by definition greater than
`iSuccE`, which is the smallest element of the
right tree. I imagine I could walk the entire
right tree to verify this to please LH like I
did with `quickSort`, but I can't imagine that
is the correct answer.

- in addition to the usual syntax:

```
     {-@ unsafeLookup :: n:Nat ->  {v:Vector a | n < (vlen v)} -> a @-}
```

- could we get a "function" syntax:

```
     {-@ unsafeLookup :: n:Nat ->  v:Vector a -> a
         unsafeLookup n v = n < vlen v @-}
```

...or...

```
     {-@ unsafeLookup :: n:Int -> v:Vector a -> a
         unsafeLookup n v = n >= 0 && n < vlen v @-}
```

...or...

```
     {-@ unsafeLookup :: n:Int -> {v:Vector a | n < (vlen v)} -> a
         unsafeLookup n v = n >= 0 @-}
```
- the "function" could take as arguments symbols that were bound
  in the "signature." It could take a partial specification and
  implement additional refinements. It "looks like Haskell" and
  allows the programmer more freedom in how they define their
  refinements
