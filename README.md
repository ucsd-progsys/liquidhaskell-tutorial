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

- Gotchas
    - hs sig vs. lh sig
    - module and import story
    - `inline`

+ Printing of predicates (we drop required parens)

? Intelligible parse errors

+ Web demo 
