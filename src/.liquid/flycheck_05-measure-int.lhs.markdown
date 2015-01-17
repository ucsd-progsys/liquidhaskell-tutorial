Numeric Measures {#numericmeasure}
================

\begin{comment}

<pre><span class=hs-linenum> 6: </span>
<span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span>
<span class=hs-linenum>11: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>NumericMeasures</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>12: </span><span class='hs-comment'>-- (</span>
<span class=hs-linenum>13: </span><span class='hs-comment'>--   -- * Types</span>
<span class=hs-linenum>14: </span><span class='hs-comment'>--     Vector (..), Matrix (..), List </span>
<span class=hs-linenum>15: </span><span class='hs-comment'>--   , dotProd, matProd</span>
<span class=hs-linenum>16: </span><span class='hs-comment'>--   , dotProduct, matProduct, transpose</span>
<span class=hs-linenum>17: </span><span class='hs-comment'>--   , vecFromList, matFromList</span>
<span class=hs-linenum>18: </span><span class='hs-comment'>--   , map, take, take', drop, for, zip, zipOrNull, partition, reverse</span>
<span class=hs-linenum>19: </span><span class='hs-comment'>--   , first, second, size, notEmpty</span>
<span class=hs-linenum>20: </span><span class='hs-comment'>--   , test1, test2, test3, test4, test5, test6, ok23, ok32, bad1, bad2 </span>
<span class=hs-linenum>21: </span><span class='hs-comment'>--   , vCons, vHd, vTl</span>
<span class=hs-linenum>22: </span><span class='hs-comment'>--   ) where</span>
<span class=hs-linenum>23: </span>
<span class=hs-linenum>24: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span>  <span class='hs-varid'>hiding</span>  <span class='hs-layout'>(</span><span class='hs-varid'>map</span><span class='hs-layout'>,</span> <span class='hs-varid'>zipWith</span><span class='hs-layout'>,</span> <span class='hs-varid'>zip</span><span class='hs-layout'>,</span> <span class='hs-varid'>take</span><span class='hs-layout'>,</span> <span class='hs-varid'>drop</span><span class='hs-layout'>,</span> <span class='hs-varid'>reverse</span><span class='hs-layout'>)</span>
<span class=hs-linenum>25: </span>
<span class=hs-linenum>26: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>die</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| false}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>27: </span><a class=annot href="#"><span class=annottext>forall a. {v : [Char] | false} -&gt; a</span><span class='hs-definition'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; a</span><span class='hs-varid'>error</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a>
<span class=hs-linenum>28: </span><span class='hs-definition'>take</span><span class='hs-layout'>,</span> <span class='hs-varid'>drop</span><span class='hs-layout'>,</span> <span class='hs-varid'>take'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>29: </span><span class='hs-definition'>txgo</span>              <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-layout'>(</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-layout'>(</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>30: </span><span class='hs-definition'>quickSort</span>         <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
</pre>

Plan
----

1. Wholemeal Programming
2. Specifying Dimensions
3. Dimension-aware List API 
4. Dimension-safe Vectors and Matrices
5. Case Study: K-Means Clustering
\end{comment}

Many of the programs we have seen so far, for example those in
[here](#vectorbounds), suffer from *indexitis*
\footnotetext{A term coined by [Richard Bird](http://www.amazon.com/Pearls-Functional-Algorithm-Design-Richard/dp/0521513383)}
a tendency to perform low-level manipulations to iterate over the
indices into a collection, which opens the door to various off-by-one
errors. Such errors can be entirely eliminated by instead programming
at a higher level, using a [wholemeal approach][hinze-icfp09]
where the emphasis is on using aggregate operations, like `map`,
`fold` and `reduce`. However, wholemeal programming requires us to
take care when operating on multiple collections; if these collections
are *incompatible*, e.g. have the wrong dimensions, then we end up with
a fate worse than a crash, a *meaningless* result.

Fortunately, LiquidHaskell can help. Lets see how we can use measures to
specify dimensions and create a dimension-aware API for lists which can be
used to implement wholemeal dimension-safe APIs.
\footnotetext{In a [later chapter](#kmeans-case-study) we will use this
API to implement K-means clustering.}


Wholemeal Programming
---------------------

Indexitis begone! As an example of wholemeal programming, lets
write a small library that represents vectors as lists and
matrices as nested vectors:


<pre><span class=hs-linenum>71: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>V</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a. (Vector a) -&gt; Int</span><span class='hs-varid'>vDim</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>72: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (Vector a) -&gt; [a]</span><span class='hs-varid'>vElts</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>73: </span>                  <span class='hs-layout'>}</span>
<span class=hs-linenum>74: </span>              <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Eq a) =&gt; (Eq (Vector a))</span><span class='hs-conid'>Eq</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>75: </span>                         
<span class=hs-linenum>76: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>M</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a. (Matrix a) -&gt; Int</span><span class='hs-varid'>mRow</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>77: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (Matrix a) -&gt; Int</span><span class='hs-varid'>mCol</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>78: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (Matrix a) -&gt; (Vector (Vector a))</span><span class='hs-varid'>mElts</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Vector</span> <span class='hs-layout'>(</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>79: </span>                  <span class='hs-layout'>}</span>
<span class=hs-linenum>80: </span>              <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Eq a) =&gt; (Eq (Matrix a))</span><span class='hs-conid'>Eq</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{Vector Product} We can write the dot product of
two `Vector`s using a fold:


<pre><span class=hs-linenum>87: </span><span class='hs-definition'>dotProd</span>       <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Num</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>88: </span><a class=annot href="#"><span class=annottext>forall a. (Num a) =&gt; (Vector a) -&gt; (Vector a) -&gt; a</span><span class='hs-definition'>dotProd</span></a> <a class=annot href="#"><span class=annottext>(Vector a)</span><span class='hs-varid'>vx</span></a> <a class=annot href="#"><span class=annottext>(Vector a)</span><span class='hs-varid'>vy</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; a</span><span class='hs-varid'>sum</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:[a]
-&gt; {v : [a] | size v == size x1} -&gt; {v : [a] | size v == size x1}</span><span class='hs-varid'>prod</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>ys</span></a></span><span class='hs-layout'>)</span>
<span class=hs-linenum>89: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>90: </span>    <a class=annot href="#"><span class=annottext>x1:[a]
-&gt; {v : [a] | size v == size x1} -&gt; {v : [a] | size v == size x1}</span><span class='hs-varid'>prod</span></a>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | size v == size x4}
-&gt; {v : [c] | size v == size x4}</span><span class='hs-varid'>zipWith</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:a
-&gt; {VV : a | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; VV &gt;= x1 &amp;&amp; VV &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; VV &gt; x1 &amp;&amp; VV &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; VV == 0}</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:a
-&gt; {VV : a | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; VV &gt;= x1 &amp;&amp; VV &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; VV &gt; x1 &amp;&amp; VV &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; VV == 0}</span><span class='hs-varop'>*</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>91: </span>    <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Vector a) -&gt; [a]</span><span class='hs-varid'>vElts</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == vx}</span><span class='hs-varid'>vx</span></a>
<span class=hs-linenum>92: </span>    <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>ys</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Vector a) -&gt; [a]</span><span class='hs-varid'>vElts</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == vy}</span><span class='hs-varid'>vy</span></a>
</pre>

\newthought{Matrix Product} Similarly, we can compute the
product of two matrices in a wholemeal fashion, without 
performing any low-level index manipulations, but instead using
a high-level "iterator" over the elements of the matrix.


<pre><span class=hs-linenum>101: </span><span class='hs-definition'>matProd</span>       <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Num</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>102: </span><a class=annot href="#"><span class=annottext>forall a. (Num a) =&gt; (Matrix a) -&gt; (Matrix a) -&gt; (Matrix a)</span><span class='hs-definition'>matProd</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>M</span> <span class='hs-varid'>rx</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>M</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>cy</span> <span class='hs-varid'>ys</span><span class='hs-layout'>)</span>
<span class=hs-linenum>103: </span>                 <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector a) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix a) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == rx &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>rx</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == cy &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>cy</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0}) | v == elts &amp;&amp; vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>elts</span></a></span>
<span class=hs-linenum>104: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>105: </span>    <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0}) | vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>elts</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Vector {v : (Vector a) | vDim v &gt; 0})
-&gt; ({v : (Vector a) | vDim v &gt; 0}
    -&gt; {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0})
-&gt; {v : (Vector {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0}) | vDim v == vDim x1}</span><span class='hs-varid'>for</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector (Vector a)) | v == xs &amp;&amp; vDim v == rx}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>(({v : (Vector a) | vDim v &gt; 0}
  -&gt; {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0})
 -&gt; {v : (Vector {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0}) | vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0})
-&gt; ({v : (Vector a) | vDim v &gt; 0}
    -&gt; {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0})
-&gt; {v : (Vector {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0}) | vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0}</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Vector a) | vDim VV &gt; 0}</span><span class='hs-varid'>xi</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>106: </span>                     <a class=annot href="#"><span class=annottext>x1:(Vector (Vector a))
-&gt; ((Vector a) -&gt; a) -&gt; {v : (Vector a) | vDim v == vDim x1}</span><span class='hs-varid'>for</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == cy}) | v == ys}</span><span class='hs-varid'>ys</span></a> <a class=annot href="#"><span class=annottext>(((Vector a) -&gt; a)
 -&gt; {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0})
-&gt; ((Vector a) -&gt; a)
-&gt; {v : (Vector a) | vDim v == vDim ys &amp;&amp; vDim v &gt; 0}</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>(Vector a)</span><span class='hs-varid'>yj</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>107: </span>                       <a class=annot href="#"><span class=annottext>(Vector a) -&gt; (Vector a) -&gt; a</span><span class='hs-varid'>dotProd</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == xi &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>xi</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == yj}</span><span class='hs-varid'>yj</span></a>
</pre>

\newthought{Iteration} In the above, the "iteration" embodied
in `for` is simply a `map` over the elements of the vector.


<pre><span class=hs-linenum>114: </span><a class=annot href="#"><span class=annottext>forall a b.
x1:(Vector b) -&gt; (b -&gt; a) -&gt; {v : (Vector a) | vDim v == vDim x1}</span><span class='hs-definition'>for</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>V</span> <span class='hs-varid'>n</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [a] | size v == x1}
-&gt; {v : (Vector a) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; x3:[a] -&gt; {v : [b] | size v == size x3}</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; size v == n &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span> 
</pre>

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
  error, we will get the wrong output. \footnotetext{In fact,
  while the implementation of  `matProd` breezes past GHC it is quite wrong!}


Specifying List Dimensions
--------------------------

In order to start reasoning about dimensions, we need a way
to represent the *dimension* of a list inside the refinement
logic. \footnotetext{We could just use `vDim`, but that is
a lazy cheat as there is no guarantee that the field's value
actually equals the size of the list!}

\newthought{Measures} are ideal for this
task. [Previously](#boolmeasures) we saw how we could lift Haskell
functions up to the refinement logic.
\footnotetext{Recall that these must be inductively defined functions,
with a single equation per data-constructor}
Lets write a measure to describe the length of a list:


<pre><span class=hs-linenum>151: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>len</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>152: </span><span class='hs-definition'>len</span>        <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>153: </span><span class='hs-definition'>len</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <span class='hs-num'>0</span>
<span class=hs-linenum>154: </span><span class='hs-definition'>len</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>1</span> <span class='hs-varop'>+</span> <span class='hs-varid'>len</span> <span class='hs-varid'>xs</span>
</pre>



<pre><span class=hs-linenum>159: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>size</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>160: </span><span class='hs-comment'>{- size    :: xs:[a] -&gt; {v:Nat | v = size xs &amp;&amp; v = len xs} @-}</span>
<span class=hs-linenum>161: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>size</span>    <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Nat</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>162: </span><span class='hs-definition'>size</span>        <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>163: </span><a class=annot href="#"><span class=annottext>forall a. [a] -&gt; {v : Int | v &gt;= 0}</span><span class='hs-definition'>size</span></a> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>rs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; {v : Int | v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == rs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>rs</span></a>
<span class=hs-linenum>164: </span><span class='hs-definition'>size</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
</pre>

\newthought{Measures Refine Constructors}
As with [refined data definitions](#autosmart), the
measures are translated into strengthened types for
the type's constructors. For example, the `size`
measure is translated into:


<pre><span class=hs-linenum>174: </span><span class='hs-keyword'>data</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>175: </span>  <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>size</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span>
<span class=hs-linenum>176: </span>  <span class='hs-layout'>(</span><span class='hs-conop'>:</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>size</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>1</span> <span class='hs-varop'>+</span> <span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>}</span>
</pre>

\newthought{Multiple Measures} We can write several
different measures for a datatype. For example, in
addition to the `size` measure, we can define a `notEmpty`
measure for the list type:


<pre><span class=hs-linenum>185: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>notEmpty</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>186: </span><span class='hs-definition'>notEmpty</span>       <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>187: </span><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {VV : Bool | Prop VV &lt;=&gt; notEmpty x1}</span><span class='hs-definition'>notEmpty</span></a> <span class='hs-conid'>[]</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
<span class=hs-linenum>188: </span><span class='hs-definition'>notEmpty</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a> 
</pre>

\newthought{Composing Measures}
LiquidHaskell lets you *compose* the different measures
simply by *conjoining* the refinements in the strengthened
constructors. For example, the two measures for lists end
up yielding the constructors:


<pre><span class=hs-linenum>198: </span><span class='hs-keyword'>data</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>199: </span>  <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>not</span> <span class='hs-layout'>(</span><span class='hs-varid'>notEmpty</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>size</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span>
<span class=hs-linenum>200: </span>  <span class='hs-layout'>(</span><span class='hs-conop'>:</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>notEmpty</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>size</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>1</span> <span class='hs-varop'>+</span> <span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>}</span>
</pre>

\noindent 
This is a very significant advantage of using measures
instead of indices as in [DML][dml] or [Agda][agdavec],
as *decouples property from structure*, which crucially
enables the use of the same structure for many different
purposes. That is, we need not know *a priori* what indices
to bake into the structure, but can define a generic
structure and refine it *a posteriori* as needed with
new measures.

Lets use `size` to create a dimension-aware API for lists.
To get the ball rolling, lets defining a few helpful type aliases:

\newthought{An `N`-List} is a list with exactly `N` elements:
\footnotetext{Note that when defining refinement type aliases,
we use uppercase variables like `N` to distinguish value- parameters
from the lowercase type parameters like `a`.}


<pre><span class=hs-linenum>222: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span> <span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>size</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent To make the signatures symmetric, lets use an alias
for plain old Lists:


<pre><span class=hs-linenum>229: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
</pre>

Lists: Size Preserving API
--------------------------

With the types firmly in hand, let us write dimension-aware
variants of the usual list functions. The implementations are
the same as in the standard library i.e. [`Data.List`][data-list];
but the specifications are enriched with dimension information.

\newthought{`map`} yields a list with the same size as the input:


<pre><span class=hs-linenum>243: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>map</span>      <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>244: </span><a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; x3:[a] -&gt; {v : [b] | size v == size x3}</span><span class='hs-definition'>map</span></a> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>245: </span><span class='hs-definition'>map</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; x3:[a] -&gt; {v : [b] | size v == size x3}</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
</pre>

\newthought{zipWith} requires both lists to have the *same* size, and produces
a list with that same size.
\footnotetext{Note that as made explicit by the call to `die`, the
input type *rules out* the case where one list is empty and the other
is not, as in that case the former's length is zero while the latter's
is not, and hence, different.}


<pre><span class=hs-linenum>256: </span>
<span class=hs-linenum>257: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>invariant</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| 0 &lt;= size v}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>258: </span>
<span class=hs-linenum>259: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zipWith</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>260: </span><a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | size v == size x4}
-&gt; {v : [c] | size v == size x4}</span><span class='hs-definition'>zipWith</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-conop'>:</span><span class='hs-keyword'>as</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-varid'>bs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == a}</span><span class='hs-varid'>a</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == b}</span><span class='hs-varid'>b</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | size v == size x4}
-&gt; {v : [c] | size v == size x4}</span><span class='hs-varid'>zipWith</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == as &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyword'>as</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == bs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>bs</span></a>
<span class=hs-linenum>261: </span><span class='hs-definition'>zipWith</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span> <span class='hs-conid'>[]</span>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>262: </span><span class='hs-definition'>zipWith</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span>  <span class='hs-keyword'>_</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; {v : [a] | false}</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"no other cases"</span></a>
</pre>

\newthought{unsafeZip} The signature for `zipWith` is quite severe -- it
rules out the case where the zipping occurs only upto the shorter input.
Here's a function that actually allows for that case, where the output
type is the *shorter* of the two inputs:


<pre><span class=hs-linenum>271: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zip</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>as</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>bs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span><span class='hs-varid'>b</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| Min (size v) (size as) (size bs)}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>272: </span><a class=annot href="#"><span class=annottext>forall a b.
x1:[a]
-&gt; x2:[b]
-&gt; {v : [(a, b)] | not (size x1 &lt; size x2) =&gt; size v == size x2 &amp;&amp; size x1 &lt; size x2 =&gt; size v == size x1}</span><span class='hs-definition'>zip</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-conop'>:</span><span class='hs-keyword'>as</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-varid'>bs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (a, b) | first v == a &amp;&amp; second v == b &amp;&amp; x_Tuple22 v == b &amp;&amp; x_Tuple21 v == a &amp;&amp; fst v == a &amp;&amp; snd v == b}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == a}</span><span class='hs-varid'>a</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == b}</span><span class='hs-varid'>b</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(a, b)
-&gt; x2:[(a, b)]
-&gt; {v : [(a, b)] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a b.
x1:[a]
-&gt; x2:[b]
-&gt; {v : [(a, b)] | not (size x1 &lt; size x2) =&gt; size v == size x2 &amp;&amp; size x1 &lt; size x2 =&gt; size v == size x1}</span><span class='hs-varid'>zip</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == as &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyword'>as</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == bs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>bs</span></a>
<span class=hs-linenum>273: </span><span class='hs-definition'>zip</span> <span class='hs-conid'>[]</span> <span class='hs-keyword'>_</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>274: </span><span class='hs-definition'>zip</span> <span class='hs-keyword'>_</span>  <span class='hs-conid'>[]</span>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a> 
</pre>

\noindent The output type uses the following which defines `X`
to be the smaller of `Y` and `Z`.
\footnotetext{Note that `if p then q else r` is simply an abbreviation for `p => q && not p => r`}


<pre><span class=hs-linenum>282: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>Min</span> <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span> <span class='hs-conid'>Z</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-keyword'>if</span> <span class='hs-conid'>X</span> <span class='hs-varop'>&lt;</span> <span class='hs-conid'>Y</span> <span class='hs-keyword'>then</span> <span class='hs-conid'>X</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Y</span> <span class='hs-keyword'>else</span> <span class='hs-conid'>X</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Z</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
</pre>

\exercisen{Zip Unless Empty} In my experience, `zip` as shown above is far too
permissive and lets all sorts of bugs into my code. As middle
ground, consider `zipOrNull` below. Write a specification
for `zipOrNull` such that the code below is verified by
LiquidHaskell:


<pre><span class=hs-linenum>292: </span><span class='hs-definition'>zipOrNull</span>       <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>293: </span><a class=annot href="#"><span class=annottext>forall a b. [a] -&gt; [b] -&gt; [(a, b)]</span><span class='hs-definition'>zipOrNull</span></a> <span class='hs-conid'>[]</span> <span class='hs-keyword'>_</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>294: </span><span class='hs-definition'>zipOrNull</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>295: </span><span class='hs-definition'>zipOrNull</span> <span class='hs-varid'>xs</span> <span class='hs-varid'>ys</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | size v == size x4}
-&gt; {v : [c] | size v == size x4}</span><span class='hs-varid'>zipWith</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:b
-&gt; {v : (a, b) | first v == x1 &amp;&amp; second v == x2 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; x_Tuple21 v == x1 &amp;&amp; fst v == x1 &amp;&amp; snd v == x2}</span><span class='hs-conid'>(,)</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>ys</span></a></span>
<span class=hs-linenum>296: </span>
<span class=hs-linenum>297: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test1</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>| size v = 2}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>298: </span><a class=annot href="#"><span class=annottext>{v : [(Integer, Bool)] | size v == 2}</span><span class='hs-definition'>test1</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>[Integer] -&gt; [Bool] -&gt; [(Integer, Bool)]</span><span class='hs-varid'>zipOrNull</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><span class='hs-num'>0</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>1</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Bool] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == GHC.Types.True &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
<span class=hs-linenum>299: </span>
<span class=hs-linenum>300: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test2</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>| size v = 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>301: </span><a class=annot href="#"><span class=annottext>forall a. {v : [(a, Bool)] | size v == 0}</span><span class='hs-definition'>test2</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>[a] -&gt; [Bool] -&gt; [(a, Bool)]</span><span class='hs-varid'>zipOrNull</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-conid'>[]</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Bool] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == GHC.Types.True &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
<span class=hs-linenum>302: </span>
<span class=hs-linenum>303: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test3</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>| size v = 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>304: </span><a class=annot href="#"><span class=annottext>forall a. {v : [([Char], a)] | size v == 0}</span><span class='hs-definition'>test3</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>[[Char]] -&gt; [a] -&gt; [([Char], a)]</span><span class='hs-varid'>zipOrNull</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [[Char]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"cat"</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"dog"</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-conid'>[]</span></a></span>
</pre>

\hint Yes, the type is rather gross; it uses a bunch of
      disjunctions `||` , conjunctions `&&` and implications `=>`.

\exercisen{Reverse} Consider the code below that reverses
a list using the tail-recursive `go`. Fix the signature for `go`
so that LiquidHaskell can prove the specification for `reverse`.


<pre><span class=hs-linenum>315: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>reverse</span>       <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| size v = size xs}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>316: </span><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : [a] | size v == size x1}</span><span class='hs-definition'>reverse</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>        <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-conid'>[]</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a></span>
<span class=hs-linenum>317: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>318: </span>    <span class='hs-keyword'>{-@</span> <span class='hs-varid'>go</span>        <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>ys</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>319: </span>    <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>acc</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == acc &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>acc</span></a>
<span class=hs-linenum>320: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>acc</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == acc &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>acc</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
</pre>

\hint How big is the list returned by `go`?

Lists: Size Reducing API {#listreducing} 
------------------------

Next, lets look at some functions that truncate lists, in one way or another.

\newthought{Take} lets us grab the first `k` elements from a list: 


<pre><span class=hs-linenum>333: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>take'</span>     <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>| n &lt;= size v}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>334: </span><a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; {VV : [a] | x1 &lt;= size VV} -&gt; {v : [a] | size v == x1}</span><span class='hs-definition'>take'</span></a> <span class='hs-num'>0</span> <span class='hs-keyword'>_</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>335: </span><span class='hs-definition'>take'</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; {VV : [a] | x1 &lt;= size VV} -&gt; {v : [a] | size v == x1}</span><span class='hs-varid'>take'</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>336: </span><span class='hs-definition'>take'</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; {v : [a] | false}</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"won't  happen"</span></a>
</pre>


\exercisen{Drop} is the yang to `take`'s yin: it returns
the remainder after extracting the first `k` elements.
Write a suitable specification for it so that the below
typechecks:


<pre><span class=hs-linenum>346: </span><a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-definition'>drop</span></a> <span class='hs-num'>0</span> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>347: </span><span class='hs-definition'>drop</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-varid'>drop</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>348: </span><span class='hs-definition'>drop</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; [a]</span><span class='hs-varid'>die</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"won't happen"</span></a></span>
<span class=hs-linenum>349: </span>
<span class=hs-linenum>350: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test4</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ListN</span> <span class='hs-conid'>String</span> <span class='hs-num'>2</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>351: </span><a class=annot href="#"><span class=annottext>{v : [[Char]] | size v == 2}</span><span class='hs-definition'>test4</span></a> <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-varid'>drop</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [[Char]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"cat"</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"dog"</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"mouse"</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span> 
</pre>

\exercisen{Take it easy} The version `take'` above is too restrictive;
it insists that the list actually have at least `n` elements.
Modify the signature for the *real* `take` function so that
the code below is accepted by LiquidHaskell:


<pre><span class=hs-linenum>360: </span><a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-definition'>take</span></a> <span class='hs-num'>0</span> <span class='hs-keyword'>_</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>361: </span><span class='hs-definition'>take</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>362: </span><span class='hs-definition'>take</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-varid'>take</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>363: </span>
<span class=hs-linenum>364: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test5</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>ListN</span> <span class='hs-conid'>String</span> <span class='hs-num'>2</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>365: </span><a class=annot href="#"><span class=annottext>[{v : [[Char]] | size v == 2}]</span><span class='hs-definition'>test5</span></a> <span class='hs-keyglyph'>=</span> <span class=hs-error><span class='hs-keyglyph'>[</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-varid'>take</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error>  </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [[Char]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"cat"</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"dog"</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"mouse"</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error>
</span><span class=hs-linenum>366: </span>        <span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a. Int -&gt; [a] -&gt; [a]</span><span class='hs-varid'>take</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (20  :  int)}</span><span class='hs-num'>20</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [[Char]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"cow"</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"goat"</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error>        </span><span class=hs-error><span class='hs-keyglyph'>]</span></span> 
</pre>

\newthought{Partition} As one last example, lets look at the
function that `partition`s a list using a user supplied predicate:


<pre><span class=hs-linenum>373: </span><a class=annot href="#"><span class=annottext>forall a.
(a -&gt; Bool)
-&gt; x3:[a]
-&gt; {v : ([a], [a]) | size first v + size second v == size x3}</span><span class='hs-definition'>partition</span></a> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x8 VV -&gt; p2 x8&gt; | first v == x1 &amp;&amp; second v == x2 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; x_Tuple21 v == x1 &amp;&amp; fst v == x1 &amp;&amp; snd v == x2}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-conid'>[]</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-conid'>[]</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>374: </span><span class='hs-definition'>partition</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>375: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>a -&gt; Bool</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x8 VV -&gt; p2 x8&gt; | first v == x1 &amp;&amp; second v == x2 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; x_Tuple21 v == x1 &amp;&amp; fst v == x1 &amp;&amp; snd v == x2}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; size v == size ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; size v == size zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>376: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x8 VV -&gt; p2 x8&gt; | first v == x1 &amp;&amp; second v == x2 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; x_Tuple21 v == x1 &amp;&amp; fst v == x1 &amp;&amp; snd v == x2}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; size v == size ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; size v == size zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>377: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>378: </span>    <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; size VV == size ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : [a] | VV == zs &amp;&amp; size VV == size zs &amp;&amp; len VV == len zs &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(a -&gt; Bool)
-&gt; x3:[a]
-&gt; {v : ([a], [a]) | size first v + size second v == size x3}</span><span class='hs-varid'>partition</span></a> <a class=annot href="#"><span class=annottext>a -&gt; Bool</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
</pre>

We would like to specify that the *sum* of the output tuple's
dimensions equal the input list's dimension.
Lets write measures to access the elements of the output:


<pre><span class=hs-linenum>386: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>first</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>387: </span><a class=annot href="#"><span class=annottext>forall a b. x1:(a, b) -&gt; {VV : a | VV == first x1}</span><span class='hs-definition'>first</span></a>  <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>
<span class=hs-linenum>388: </span>
<span class=hs-linenum>389: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>second</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>390: </span><a class=annot href="#"><span class=annottext>forall a b. x1:(a, b) -&gt; {VV : b | VV == second x1}</span><span class='hs-definition'>second</span></a> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-layout'>,</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>
</pre>

\noindent We can use the above to type `partition` as


<pre><span class=hs-linenum>396: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>partition</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ListPair</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent using an alias for a pair of lists whose total dimension equals `N`


<pre><span class=hs-linenum>402: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ListPair</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-layout'>(</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>size</span> <span class='hs-layout'>(</span><span class='hs-varid'>first</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-varid'>size</span> <span class='hs-layout'>(</span><span class='hs-varid'>second</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\exercisen{QuickSort} Use the `partition` function above to implement `quickSort`:


<pre><span class=hs-linenum>408: </span><span class='hs-comment'>-- &gt;&gt; quickSort [1,4,3,2]</span>
<span class=hs-linenum>409: </span><span class='hs-comment'>-- [1,2,3,4]</span>
<span class=hs-linenum>410: </span>
<span class=hs-linenum>411: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>quickSort</span>    <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>size</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>412: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; x2:[a] -&gt; {v : [a] | size v == size x2}</span><span class='hs-definition'>quickSort</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x6 VV -&gt; p x6&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; size v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>413: </span><span class='hs-definition'>quickSort</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>
<span class=hs-linenum>414: </span>
<span class=hs-linenum>415: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test10</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ListN</span> <span class='hs-conid'>String</span> <span class='hs-num'>2</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>416: </span><a class=annot href="#"><span class=annottext>{v : [[Char]] | size v == 2}</span><span class='hs-definition'>test10</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[[Char]] -&gt; {v : [[Char]] | size v == size x1}</span><span class='hs-varid'>quickSort</span></a> <a class=annot href="#"><span class=annottext>{v : [[Char]] | v == NumericMeasures.test4 &amp;&amp; size v == 2 &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>test4</span></a> 
</pre>


Dimension Safe Vector API
-------------------------

We can use the dimension aware lists to create a safe vector API.

\newthought{Legal Vectors} are those whose `vDim` field actually equals the size of the underlying list:


<pre><span class=hs-linenum>428: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>V</span> <span class='hs-layout'>{</span> <span class='hs-varid'>vDim</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Nat</span>
<span class=hs-linenum>429: </span>                      <span class='hs-layout'>,</span> <span class='hs-varid'>vElts</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ListN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>vDim</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>430: </span>  <span class='hs-keyword'>@-}</span>
</pre>

\noindent 
The refined data type prevents the creation of illegal vectors:


<pre><span class=hs-linenum>437: </span><a class=annot href="#"><span class=annottext>(Vector Integer)</span><span class='hs-definition'>okVec</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>10</span><span class='hs-layout'>,</span> <span class='hs-num'>20</span><span class='hs-keyglyph'>]</span>       <span class='hs-comment'>-- accepted by LH</span>
<span class=hs-linenum>438: </span>
<span class=hs-linenum>439: </span><a class=annot href="#"><span class=annottext>(Vector Integer)</span><span class='hs-definition'>badVec</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><span class='hs-num'>10</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>20</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>30</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span>   <span class='hs-comment'>-- rejected by LH</span>
</pre>

\newthought{Access} Next, lets write some functions to access the elements of a vector:


<pre><span class=hs-linenum>445: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>vCons</span>        <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>| vDim v = vDim x + 1}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>446: </span><a class=annot href="#"><span class=annottext>forall a.
a -&gt; x2:(Vector a) -&gt; {v : (Vector a) | vDim v == vDim x2 + 1}</span><span class='hs-definition'>vCons</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>V</span> <span class='hs-varid'>n</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [a] | size v == x1}
-&gt; {v : (Vector a) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; size v == 1 + size x2 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; size v == n &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>447: </span>
<span class=hs-linenum>448: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>VectorNE</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>vDim</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&gt;</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>449: </span>
<span class=hs-linenum>450: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>vHd</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>VectorNE</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>451: </span><a class=annot href="#"><span class=annottext>forall a. {v : (Vector a) | vDim v &gt; 0} -&gt; a</span><span class='hs-definition'>vHd</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>V</span> <span class='hs-keyword'>_</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>
<span class=hs-linenum>452: </span><span class='hs-definition'>vHd</span> <span class='hs-keyword'>_</span>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"nope"</span></a>
<span class=hs-linenum>453: </span>
<span class=hs-linenum>454: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>vTl</span>          <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>VectorNE</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>| vDim v = vDim x - 1}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>455: </span><a class=annot href="#"><span class=annottext>forall a.
x1:{v : (Vector a) | vDim v &gt; 0}
-&gt; {v : (Vector a) | vDim v == vDim x1 - 1}</span><span class='hs-definition'>vTl</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>V</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [a] | size v == x1}
-&gt; {v : (Vector a) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a> 
<span class=hs-linenum>456: </span><span class='hs-definition'>vTl</span> <span class='hs-keyword'>_</span>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; {v : (Vector a) | false}</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-str'>"nope"</span></a>
</pre>

\newthought{Iteration} It is straightforward to see that:


<pre><span class=hs-linenum>462: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>for</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>VectorN</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>vDim</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
</pre>
\newthought{Binary Operations} We want to apply various binary
operations to *compatible* vectors, i.e. vectors with equal
dimensions. To this end, it is handy to have an alias for
vectors of a given size:


<pre><span class=hs-linenum>470: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>VectorN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>vDim</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent We can now write a generic binary operator:


<pre><span class=hs-linenum>476: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>vBin</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>vx</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>vy</span><span class='hs-conop'>:</span><span class='hs-conid'>VectorN</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>vDim</span> <span class='hs-varid'>vx</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>VectorN</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-varid'>vDim</span> <span class='hs-varid'>vx</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>477: </span><span class='hs-definition'>vBin</span>     <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>c</span>
<span class=hs-linenum>478: </span><a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:(Vector a)
-&gt; {v : (Vector b) | vDim v == vDim x4}
-&gt; {v : (Vector c) | vDim v == vDim x4}</span><span class='hs-definition'>vBin</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>op</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>V</span> <span class='hs-varid'>n</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>V</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>ys</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [a] | size v == x1}
-&gt; {v : (Vector a) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | size v == size x4}
-&gt; {v : [c] | size v == size x4}</span><span class='hs-varid'>zipWith</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>op</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; size v == n &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{Dot Product} Finally, we can implement a wholemeal,
dimension safe dot product operator as:


<pre><span class=hs-linenum>485: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>dotProduct</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Num</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-conid'>VectorN</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>vDim</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>486: </span><a class=annot href="#"><span class=annottext>forall a.
(Num a) =&gt;
x2:(Vector a) -&gt; {v : (Vector a) | vDim v == vDim x2} -&gt; a</span><span class='hs-definition'>dotProduct</span></a> <a class=annot href="#"><span class=annottext>(Vector a)</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | vDim v == vDim x}</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; a</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>([a] -&gt; a) -&gt; [a] -&gt; a</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>(Vector a) -&gt; [a]</span><span class='hs-varid'>vElts</span></a> <a class=annot href="#"><span class=annottext>((Vector a) -&gt; [a]) -&gt; (Vector a) -&gt; [a]</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>(a -&gt; a -&gt; a)
-&gt; x4:(Vector a)
-&gt; (Vector a)
-&gt; {v : (Vector a) | vDim v == vDim x4}</span><span class='hs-varid'>vBin</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:a
-&gt; {VV : a | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; VV &gt;= x1 &amp;&amp; VV &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; VV &gt; x1 &amp;&amp; VV &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; VV == 0}</span><span class='hs-layout'>(</span></a><span class='hs-varop'>*</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == y &amp;&amp; vDim v == vDim x}</span><span class='hs-varid'>y</span></a> 
</pre>

\exercisen{Vector Constructor} Complete the *specification* and
*implementation* of `vecFromList` which *creates* a `Vector` from
a plain old list.


<pre><span class=hs-linenum>494: </span><span class='hs-definition'>vecFromList</span>     <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Vector</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>495: </span><a class=annot href="#"><span class=annottext>forall a. [a] -&gt; (Vector a)</span><span class='hs-definition'>vecFromList</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>
<span class=hs-linenum>496: </span>
<span class=hs-linenum>497: </span><a class=annot href="#"><span class=annottext>Integer</span><span class='hs-definition'>test6</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Vector Integer)
-&gt; {v : (Vector Integer) | vDim v == vDim x1} -&gt; Integer</span><span class='hs-varid'>dotProduct</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector Integer) | v == vx}</span><span class='hs-varid'>vx</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Vector Integer) | v == vy}</span><span class='hs-varid'>vy</span></a></span>    <span class='hs-comment'>-- should be accepted by LH</span>
<span class=hs-linenum>498: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>499: </span>    <a class=annot href="#"><span class=annottext>(Vector Integer)</span><span class='hs-varid'>vx</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Integer] -&gt; (Vector Integer)</span><span class='hs-varid'>vecFromList</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>1</span><span class='hs-layout'>,</span><span class='hs-num'>2</span><span class='hs-layout'>,</span><span class='hs-num'>3</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>500: </span>    <a class=annot href="#"><span class=annottext>(Vector Integer)</span><span class='hs-varid'>vy</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Integer] -&gt; (Vector Integer)</span><span class='hs-varid'>vecFromList</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>4</span><span class='hs-layout'>,</span><span class='hs-num'>5</span><span class='hs-layout'>,</span><span class='hs-num'>6</span><span class='hs-keyglyph'>]</span>
</pre>

Dimension Safe Matrix API 
-------------------------

The same methods let us create a dimension safe Matrix API which
ensures that only legal matrices are created and that operations
are performed on compatible matrices.

\newthought{Legal Matrices} are those where the dimension of the
outer vector equals the number of rows `mRow` and the dimension
of each inner vector is `mCol`. We can specify legality in a
refined data definition: 


<pre><span class=hs-linenum>516: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>M</span> <span class='hs-layout'>{</span> <span class='hs-varid'>mRow</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Pos</span>
<span class=hs-linenum>517: </span>                      <span class='hs-layout'>,</span> <span class='hs-varid'>mCol</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Pos</span> 
<span class=hs-linenum>518: </span>                      <span class='hs-layout'>,</span> <span class='hs-varid'>mElts</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>VectorN</span> <span class='hs-layout'>(</span><span class='hs-conid'>VectorN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>mCol</span><span class='hs-layout'>)</span> <span class='hs-varid'>mRow</span>
<span class=hs-linenum>519: </span>                      <span class='hs-layout'>}</span>
<span class=hs-linenum>520: </span>  <span class='hs-keyword'>@-}</span>
</pre>

\noindent Notice that we avoid disallow degenerate matrices by
requiring the dimensions to be positive.


<pre><span class=hs-linenum>527: </span>
<span class=hs-linenum>528: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Pos</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent It is convenient to have an alias for matrices of a given size:


<pre><span class=hs-linenum>534: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>MatrixN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>R</span> <span class='hs-conid'>C</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>mRow</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>R</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>mCol</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>C</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent after LiquidHaskell accepts:


<pre><span class=hs-linenum>540: </span><a class=annot href="#"><span class=annottext>(Matrix Integer)</span><span class='hs-definition'>ok23</span></a>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector Integer) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix Integer) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [{v : (Vector Integer) | vDim v &gt; 0}] | size v == x1}
-&gt; {v : (Vector {v : (Vector Integer) | vDim v &gt; 0}) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <span class='hs-keyglyph'>[</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>1</span><span class='hs-layout'>,</span> <span class='hs-num'>2</span><span class='hs-layout'>,</span> <span class='hs-num'>3</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>541: </span>                       <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>4</span><span class='hs-layout'>,</span> <span class='hs-num'>5</span><span class='hs-layout'>,</span> <span class='hs-num'>6</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span>
</pre>

\exercisen{Legal Matrix} Modify the definitions of `bad1` and `bad2`
so that they are legal matrices accepted by LiquidHaskell.


<pre><span class=hs-linenum>548: </span><span class='hs-definition'>bad1</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>549: </span><a class=annot href="#"><span class=annottext>(Matrix Int)</span><span class='hs-definition'>bad1</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector Int) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix Int) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [{v : (Vector Int) | vDim v &gt; 0}] | size v == x1}
-&gt; {v : (Vector {v : (Vector Int) | vDim v &gt; 0}) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <span class='hs-keyglyph'>[</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Int] | size v == x1}
-&gt; {v : (Vector Int) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error>   </span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
<span class=hs-linenum>550: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Int] | size v == x1}
-&gt; {v : (Vector Int) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (6  :  int)}</span><span class='hs-num'>6</span></a><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span>
<span class=hs-linenum>551: </span>
<span class=hs-linenum>552: </span><span class='hs-definition'>bad2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>553: </span><a class=annot href="#"><span class=annottext>(Matrix Int)</span><span class='hs-definition'>bad2</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector Int) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix Int) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [{v : (Vector Int) | vDim v &gt; 0}] | size v == x1}
-&gt; {v : (Vector {v : (Vector Int) | vDim v &gt; 0}) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-keyglyph'>[</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Int] | size v == x1}
-&gt; {v : (Vector Int) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error>
</span><span class=hs-linenum>554: </span>                  <span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Int] | size v == x1}
-&gt; {v : (Vector Int) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class='hs-layout'>)</span>
</pre>

\exercisen{Matrix Constructor} \singlestar Write a function to construct a `Matrix` from a nested list.


<pre><span class=hs-linenum>560: </span><span class='hs-definition'>matFromList</span>      <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-layout'>(</span><span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>561: </span><a class=annot href="#"><span class=annottext>forall a. [[a]] -&gt; (Maybe (Matrix a))</span><span class='hs-definition'>matFromList</span></a> <span class='hs-conid'>[]</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Maybe a) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a>                       <span class='hs-comment'>-- no meaningful dimensions! </span>
<span class=hs-linenum>562: </span><span class='hs-definition'>matFromList</span> <span class='hs-varid'>xss</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span>
<span class=hs-linenum>563: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Bool | false}</span><span class='hs-varid'>ok</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Matrix a)
-&gt; {v : (Maybe (Matrix a)) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector a) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix a) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == r &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>r</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == c &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | false}) | false}</span><span class='hs-varid'>vs</span></a><span class='hs-layout'>)</span> 
<span class=hs-linenum>564: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (Maybe (Matrix a)) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a> 
<span class=hs-linenum>565: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>566: </span>    <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>r</span></a>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; {v : Int | v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [[a]] | len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xss</span></a>
<span class=hs-linenum>567: </span>    <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>c</span></a>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; {v : Int | v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>568: </span>    <a class=annot href="#"><span class=annottext>forall a. {VV : a | false}</span><span class='hs-varid'>ok</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | false}</span><span class='hs-varid'>undefined</span></a>
<span class=hs-linenum>569: </span>    <a class=annot href="#"><span class=annottext>forall a. {VV : a | false}</span><span class='hs-varid'>vs</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | false}</span><span class='hs-varid'>undefined</span></a> 
<span class=hs-linenum>570: </span>
</pre>

\exercisen{Refined Matrix Constructor} \doublestar Refine the
specification for `matFromList` so that the following is
accepted by LiquidHaskell:


<pre><span class=hs-linenum>578: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>mat23</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Maybe</span> <span class='hs-layout'>(</span><span class='hs-conid'>MatrixN</span> <span class='hs-conid'>Integer</span> <span class='hs-num'>2</span> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>579: </span><a class=annot href="#"><span class=annottext>(Maybe {v : (Matrix Integer) | mRow v == 2 &amp;&amp; mCol v == 2})</span><span class='hs-definition'>mat23</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>[[Integer]] -&gt; (Maybe (Matrix Integer))</span><span class='hs-varid'>matFromList</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-keyglyph'>[</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><span class='hs-num'>1</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>2</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error>
</span><span class=hs-linenum>580: </span>                        <span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><span class='hs-num'>3</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>4</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
</pre>

\hint It is easy to specify the number of rows from `xss`.
How will you figure out the number of columns? A measure
may be useful.

\begin{comment}
-- DELETE ME
{-@ matFromList  :: xss:[[a]] -> Maybe (MatrixN a (size xss) (cols xss)) @-}
{-@ measure cols @-}
{-@ cols   :: [[a]] -> Nat @-}
cols (r:_) = size r
cols []    = 0
\end{comment}

\newthought{Matrix Multiplication} Ok, lets now implement
matrix multiplication. You'd think we did it already, but
in fact the implementation at the top of this chapter
is all wrong.
\footnotetext{You could run it of course, or you could
just replace `dotProd` with our type-safe `dotProduct`
and see what happens!}
Indeed, you cannot just multiply any two matrices: the
number of *columns* of the first must equal to the *rows*
of the second -- after which point the result comprises
the `dotProduct` of the rows of the first matrix with
the columns of the second.


<pre><span class=hs-linenum>610: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>matProduct</span>   <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Num</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>611: </span>                            <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-keyword'>{Matrix a  | mCol x = mRow y}</span>
<span class=hs-linenum>612: </span>                            <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>MatrixN</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>mRow</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>mCol</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span>
<span class=hs-linenum>613: </span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>614: </span><a class=annot href="#"><span class=annottext>forall a.
(Num a) =&gt;
x2:(Matrix a)
-&gt; x3:{y : (Matrix a) | mCol x2 == mRow y}
-&gt; {v : (Matrix a) | mRow v == mRow x2 &amp;&amp; mCol v == mCol x3}</span><span class='hs-definition'>matProduct</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>M</span> <span class='hs-varid'>rx</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(Matrix a)</span><span class='hs-varid'>my</span></a><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>M</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>cy</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>
<span class=hs-linenum>615: </span>                 <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector a) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix a) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == rx &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>rx</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == cy &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>cy</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0}) | v == elts &amp;&amp; vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>elts</span></a>
<span class=hs-linenum>616: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>617: </span>    <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0}) | vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>elts</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Vector {v : (Vector a) | vDim v &gt; 0})
-&gt; ({v : (Vector a) | vDim v &gt; 0}
    -&gt; {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0})
-&gt; {v : (Vector {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0}) | vDim v == vDim x1}</span><span class='hs-varid'>for</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector (Vector a)) | v == xs &amp;&amp; vDim v == rx}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>(({v : (Vector a) | vDim v &gt; 0}
  -&gt; {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0})
 -&gt; {v : (Vector {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0}) | vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0})
-&gt; ({v : (Vector a) | vDim v &gt; 0}
    -&gt; {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0})
-&gt; {v : (Vector {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0}) | vDim v == rx &amp;&amp; vDim v == vDim xs &amp;&amp; vDim v &gt; 0}</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Vector a) | vDim VV &gt; 0}</span><span class='hs-varid'>xi</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>618: </span>                     <a class=annot href="#"><span class=annottext>x1:(Vector {v : (Vector a) | vDim v == vDim xi &amp;&amp; vDim v &gt; 0})
-&gt; ({v : (Vector a) | vDim v == vDim xi &amp;&amp; vDim v &gt; 0} -&gt; a)
-&gt; {v : (Vector a) | vDim v == vDim x1}</span><span class='hs-varid'>for</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v &gt; 0}) | v == ys' &amp;&amp; vDim v == cy &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>ys'</span></a> <a class=annot href="#"><span class=annottext>(({v : (Vector a) | vDim v == vDim xi &amp;&amp; vDim v &gt; 0} -&gt; a)
 -&gt; {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0})
-&gt; ({v : (Vector a) | vDim v == vDim xi &amp;&amp; vDim v &gt; 0} -&gt; a)
-&gt; {v : (Vector a) | vDim v == cy &amp;&amp; vDim v == vDim fix &amp;&amp; vDim v &gt; 0}</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Vector a) | vDim VV == vDim xi &amp;&amp; vDim VV &gt; 0}</span><span class='hs-varid'>yj</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>619: </span>                       <a class=annot href="#"><span class=annottext>x1:(Vector a) -&gt; {v : (Vector a) | vDim v == vDim x1} -&gt; a</span><span class='hs-varid'>dotProduct</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == xi &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>xi</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector a) | v == yj &amp;&amp; vDim v == vDim xi &amp;&amp; vDim v &gt; 0}</span><span class='hs-varid'>yj</span></a>
<span class=hs-linenum>620: </span>    <span class='hs-conid'>M</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <a class=annot href="#"><span class=annottext>{VV : (Vector {VV : (Vector a) | vDim VV &gt; 0}) | vDim VV == cy &amp;&amp; vDim VV &gt; 0}</span><span class='hs-varid'>ys'</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Matrix a)
-&gt; {v : (Matrix a) | mRow v == mCol x1 &amp;&amp; mCol v == mRow x1}</span><span class='hs-varid'>transpose</span></a> <a class=annot href="#"><span class=annottext>{v : (Matrix a) | v == my &amp;&amp; mCol v == cy}</span><span class='hs-varid'>my</span></a> 
</pre>

\newthought{Transposition} To iterate over the columns of
`my` we just `transpose` it so the columns become rows.


<pre><span class=hs-linenum>627: </span><span class='hs-comment'>-- &gt;&gt;&gt; ok32 == transpose ok23</span>
<span class=hs-linenum>628: </span><span class='hs-comment'>-- True</span>
<span class=hs-linenum>629: </span><a class=annot href="#"><span class=annottext>(Matrix Integer)</span><span class='hs-definition'>ok32</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector Integer) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix Integer) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [{v : (Vector Integer) | vDim v &gt; 0}] | size v == x1}
-&gt; {v : (Vector {v : (Vector Integer) | vDim v &gt; 0}) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <span class='hs-keyglyph'>[</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>1</span><span class='hs-layout'>,</span> <span class='hs-num'>4</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>630: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>2</span><span class='hs-layout'>,</span> <span class='hs-num'>5</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>631: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : [Integer] | size v == x1}
-&gt; {v : (Vector Integer) | vElts v == x2 &amp;&amp; vDim v == x1}</span><span class='hs-conid'>V</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0 &amp;&amp; 0 &lt;= size v}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>3</span><span class='hs-layout'>,</span> <span class='hs-num'>6</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span>
</pre>

\exercisen{Matrix Transposition} \doublestar
Use the `Vector` API to Complete the implementation of `txgo`.
For inspiration, you might look at the implementation of
`Data.List.transpose` from the [prelude][URL-transpose].
Better still, don't.


<pre><span class=hs-linenum>641: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>transpose</span>          <span class='hs-keyglyph'>::</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>MatrixN</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>mCol</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>mRow</span> <span class='hs-varid'>m</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>642: </span><a class=annot href="#"><span class=annottext>forall a.
x1:(Matrix a)
-&gt; {v : (Matrix a) | mRow v == mCol x1 &amp;&amp; mCol v == mRow x1}</span><span class='hs-definition'>transpose</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>M</span> <span class='hs-varid'>r</span> <span class='hs-varid'>c</span> <span class='hs-varid'>rows</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | 0 &lt; v}
-&gt; x2:{v : Int | 0 &lt; v}
-&gt; x3:{v : (Vector {v : (Vector a) | vDim v == x2}) | vDim v == x1}
-&gt; {v : (Matrix a) | mElts v == x3 &amp;&amp; mRow v == x1 &amp;&amp; mCol v == x2}</span><span class='hs-conid'>M</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == c &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == r &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>r</span></a> <a class=annot href="#"><span class=annottext>({v : (Vector {v : (Vector a) | vDim v == r &amp;&amp; vDim v == vDim rows &amp;&amp; vDim v &gt; 0}) | vDim v == c &amp;&amp; vDim v &gt; 0}
 -&gt; (Matrix a))
-&gt; {v : (Vector {v : (Vector a) | vDim v == r &amp;&amp; vDim v == vDim rows &amp;&amp; vDim v &gt; 0}) | vDim v == c &amp;&amp; vDim v &gt; 0}
-&gt; (Matrix a)</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : Int | v &gt;= 0}
-&gt; {v : (Vector {v : (Vector a) | vDim v == x1}) | vDim v == x2}
-&gt; {v : (Vector {v : (Vector a) | vDim v == x2}) | vDim v == x1}</span><span class='hs-varid'>txgo</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == c &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == r &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>r</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == c}) | v == rows &amp;&amp; vDim v == r}</span><span class='hs-varid'>rows</span></a>
<span class=hs-linenum>643: </span>
<span class=hs-linenum>644: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>txgo</span>      <span class='hs-keyglyph'>::</span> <span class='hs-varid'>c</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>r</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span>
<span class=hs-linenum>645: </span>              <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>VectorN</span> <span class='hs-layout'>(</span><span class='hs-conid'>VectorN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-varid'>r</span>
<span class=hs-linenum>646: </span>              <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>VectorN</span> <span class='hs-layout'>(</span><span class='hs-conid'>VectorN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-varid'>c</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>647: </span><a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : Int | v &gt;= 0}
-&gt; {v : (Vector {v : (Vector a) | vDim v == x1}) | vDim v == x2}
-&gt; {v : (Vector {v : (Vector a) | vDim v == x2}) | vDim v == x1}</span><span class='hs-definition'>txgo</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>r</span></a> <a class=annot href="#"><span class=annottext>{v : (Vector {v : (Vector a) | vDim v == c}) | vDim v == r}</span><span class='hs-varid'>rows</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>
</pre>

\hint As shown by `ok23` and `ok32`, `transpose` works by
stripping out the `head`s of the input rows, to create the
corresponding output rows.

Recap
-----

In this chapter, we saw how to use measures to describe
numeric properties of structures like lists (`Vector`)
and nested lists (`Matrix`). To recap:

1. Measures are *structurally recursive* functions, with a single
   equation per data constructor,

2. Measures can be used to create refined data definitions
   that prevent the creation of illegal values,

3. Measures can then be used to enable safe wholemeal programming,
   via dimension-aware APIs that ensure that operators only apply to
   compatible values. 

We can use numeric measures to encode various other properties
of structures; in subsequent chapters we will see examples ranging
from high-level [height-balanced trees](#case-study-wbl), to low-level
safe [pointer arithmetic](#case-study-pointers).


