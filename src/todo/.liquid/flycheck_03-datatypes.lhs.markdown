Refined Data Types
==================

 
\begin{comment}

<pre><span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span>
<span class=hs-linenum>11: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>SparseVectors</span> <span class='hs-conid'>()</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>12: </span>
<span class=hs-linenum>13: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span>      <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>abs</span><span class='hs-layout'>,</span> <span class='hs-varid'>length</span><span class='hs-layout'>)</span>
<span class=hs-linenum>14: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>List</span>    <span class='hs-layout'>(</span><span class='hs-varid'>foldl'</span><span class='hs-layout'>)</span>
<span class=hs-linenum>15: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Vector</span>  <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>foldl'</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>16: </span>
</pre>
\end{comment}



Refining Data Types: Sparse Vectors
-----------------------------------

While the standard `Vector` is great for *dense* arrays,
often we have to manipulate *sparse* vectors where most
elements are just `0`. We might represent such vectors
as a list of index-value tuples:


<pre><span class=hs-linenum>31: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>SP</span> <span class='hs-layout'>{</span> <span class='hs-varid'>spSize</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>32: </span>                   <span class='hs-layout'>,</span> <span class='hs-varid'>spElems</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-layout'>}</span> 
</pre>

\noindent Implicitly, all indices *other* than those in the list
have the value `0` (or the equivalent value for the type `a`).

\newthought{Data Invariants} Unfortunately, Haskell's type system
does not make it easy to represent the fact that every *legal* `Sparse`
vector has indices that are between `0` and `spSize`. Fortunately, this is
easy to describe as a data type refinement in LiquidHaskell:


<pre><span class=hs-linenum>44: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>SP</span> <span class='hs-layout'>{</span> <span class='hs-varid'>spSize</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Nat</span> 
<span class=hs-linenum>45: </span>                       <span class='hs-layout'>,</span> <span class='hs-varid'>spElems</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Btwn</span> <span class='hs-num'>0</span> <span class='hs-varid'>spSize</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent In the above, we specify that `spSize` is non-negative,
and each index is indeed valid. Consequently LiquidHaskell verifies:


<pre><span class=hs-linenum>52: </span><span class='hs-definition'>okSP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>53: </span><span class='hs-definition'>okSP</span><span class='hs-keyglyph'>=</span> <span class='hs-conid'>SP</span> <span class='hs-num'>5</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-num'>0</span><span class='hs-layout'>,</span> <span class='hs-str'>"cat"</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-num'>3</span><span class='hs-layout'>,</span> <span class='hs-str'>"dog"</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
</pre>

\noindent but rejects:


<pre><span class=hs-linenum>59: </span><span class='hs-definition'>badSP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>60: </span><span class='hs-definition'>badSP</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>SP</span> <span class='hs-num'>5</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-num'>0</span><span class='hs-layout'>,</span> <span class='hs-str'>"cat"</span><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-num'>6</span><span class='hs-layout'>,</span> <span class='hs-str'>"dog"</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
</pre>

\newthought{Field Measures} It is convenient to write an alias
for sparse vectors of a given size `N`; note that the field name
`spSize` are *measures*, like `vlen`, and can be used inside
refinements:


<pre><span class=hs-linenum>69: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>SparseN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>spSize</span> <span class='hs-varid'>v</span> <span class='hs-varop'>==</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span> 
</pre>

\noindent The alias `SparseN` is just a 
shorthand for the (longer) type on the right, it does not
*define* a new type. If you are familiar with the *index-style*
length encoding e.g. as found in [DML][dml] or [Agda][agdavec],
then note that despite  appearances, our `Sparse` definition
is *not* indexed.

\newthought{Sparse Products}
Lets write a function to compute a sparse product


<pre><span class=hs-linenum>83: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>sparseProduct</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>SparseN</span> <span class='hs-conid'>Int</span> <span class='hs-layout'>(</span><span class='hs-varid'>vlen</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>84: </span><span class='hs-definition'>sparseProduct</span> <span class='hs-varid'>x</span> <span class='hs-layout'>(</span><span class='hs-conid'>SP</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>go</span> <span class='hs-num'>0</span> <span class='hs-varid'>y</span>
<span class=hs-linenum>85: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>86: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>sum</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>y'</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>go</span> <span class='hs-layout'>(</span><span class='hs-varid'>sum</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-varop'>!</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-varop'>*</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varid'>y'</span> 
<span class=hs-linenum>87: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>sum</span> <span class='hs-conid'>[]</span>            <span class='hs-keyglyph'>=</span> <span class='hs-varid'>sum</span>
</pre>

LiquidHaskell verifies the above by using the specification
to conclude that for each tuple `(i, v)` in the list `y`, the
value of `i` is within the bounds of the vector `x`, thereby
proving `x ! i` safe.

Refinements and Polymorphism
----------------------------

The sharp reader will have undoubtedly noticed that the sparse product 
can be more cleanly expressed as a [fold][foldl]:


<pre><span class=hs-linenum>102: </span><span class='hs-definition'>foldl'</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>
</pre>

\noindent We can simply fold over the sparse vector, accumulating the `sum`
as we go along


<pre><span class=hs-linenum>109: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>sparseProduct'</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>SparseN</span> <span class='hs-conid'>Int</span> <span class='hs-layout'>(</span><span class='hs-varid'>vlen</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>110: </span><span class='hs-definition'>sparseProduct'</span> <span class='hs-varid'>x</span> <span class='hs-layout'>(</span><span class='hs-conid'>SP</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>foldl'</span> <span class='hs-varid'>body</span> <span class='hs-num'>0</span> <span class='hs-varid'>y</span>   
<span class=hs-linenum>111: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>112: </span>    <span class='hs-varid'>body</span> <span class='hs-varid'>sum</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span>       <span class='hs-keyglyph'>=</span> <span class='hs-varid'>sum</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-varop'>!</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span>  <span class='hs-varop'>*</span> <span class='hs-varid'>v</span>
</pre>

\noindent
LiquidHaskell digests this too, without much difficulty. 
The main trick is in how the polymorphism of `foldl'` is instantiated. 

1. The GHC type inference engine deduces that at this site,
   the type variable `b` from the signature of `foldl'` is
   instantiated to the Haskell type `(Int, a)`. 

2. Correspondingly, LiquidHaskell infers that in fact `b`
   can be instantiated to the *refined* `(Btwn 0 v (vlen x), a)`. 

Thus, the inference mechanism saves us a fair bit of typing and allows us to
reuse existing polymorphic functions over containers and such without ceremony.

\newthought{Thats all} for now! Hopefully the above gives you
a reasonable idea of how one can use refinements to verify size
related properties, and more generally, to specify and verify
properties of recursive, and polymorphic functions operating
over datatypes. Read on to learn how we can teach LiquidHaskell
to reason about *structural* properties of data types.

[vecspec]:  https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/Data/Vector.spec
[vec]:      http://hackage.haskell.org/package/vector
[dml]:      http://www.cs.bu.edu/~hwxi/DML/DML.html
[agdavec]:  http://code.haskell.org/Agda/examples/Vec.agda
[ref101]:   /blog/2013/01/01/refinement-types-101.lhs/ 
[ref102]:   /blog/2013/01/27/refinements-101-reax.lhs/ 
[foldl]:    http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-List.html
[listtail]: /blog/2013/01/31/safely-catching-a-list-by-its-tail.lhs/
[dmlarray]: http://www.cs.bu.edu/~hwxi/academic/papers/pldi98.pdf

