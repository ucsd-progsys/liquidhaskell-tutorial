Refined Datatypes {#refineddatatypes}
=================

 
\begin{comment}

<pre><span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span>
<span class=hs-linenum>11: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>RefinedDatatypes</span>
<span class=hs-linenum>12: </span>       <span class='hs-layout'>(</span>
<span class=hs-linenum>13: </span>         <span class='hs-comment'>-- * Sparse: Data</span>
<span class=hs-linenum>14: </span>         <span class='hs-conid'>Sparse</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>..</span><span class='hs-layout'>)</span>
<span class=hs-linenum>15: </span>
<span class=hs-linenum>16: </span>         <span class='hs-comment'>-- * Sparse: Functions</span>
<span class=hs-linenum>17: </span>       <span class='hs-layout'>,</span> <span class='hs-varid'>dotProd</span><span class='hs-layout'>,</span> <span class='hs-varid'>dotProd'</span><span class='hs-layout'>,</span> <span class='hs-varid'>plus</span><span class='hs-layout'>,</span> <span class='hs-varid'>fromList</span>  
<span class=hs-linenum>18: </span>
<span class=hs-linenum>19: </span>         <span class='hs-comment'>-- * Sparse: Examples</span>
<span class=hs-linenum>20: </span>       <span class='hs-layout'>,</span> <span class='hs-varid'>okSP</span><span class='hs-layout'>,</span> <span class='hs-varid'>badSP</span><span class='hs-layout'>,</span> <span class='hs-varid'>test1</span><span class='hs-layout'>,</span> <span class='hs-varid'>test2</span>
<span class=hs-linenum>21: </span> 
<span class=hs-linenum>22: </span>          <span class='hs-comment'>-- * OrdList: Data</span>
<span class=hs-linenum>23: </span>       <span class='hs-layout'>,</span> <span class='hs-conid'>IncList</span>  <span class='hs-layout'>(</span><span class='hs-keyglyph'>..</span><span class='hs-layout'>)</span>
<span class=hs-linenum>24: </span>
<span class=hs-linenum>25: </span>          <span class='hs-comment'>-- * OrdList: Examples</span>
<span class=hs-linenum>26: </span>       <span class='hs-layout'>,</span> <span class='hs-varid'>okList</span><span class='hs-layout'>,</span> <span class='hs-varid'>badList</span> 
<span class=hs-linenum>27: </span>
<span class=hs-linenum>28: </span>          <span class='hs-comment'>-- * OrdList: Functions</span>
<span class=hs-linenum>29: </span>       <span class='hs-layout'>,</span>  <span class='hs-varid'>insertSort</span><span class='hs-layout'>,</span> <span class='hs-varid'>insertSort'</span><span class='hs-layout'>,</span> <span class='hs-varid'>mergeSort</span><span class='hs-layout'>,</span> <span class='hs-varid'>quickSort</span>
<span class=hs-linenum>30: </span>
<span class=hs-linenum>31: </span>          <span class='hs-comment'>-- * BST: Data</span>
<span class=hs-linenum>32: </span>       <span class='hs-layout'>,</span> <span class='hs-conid'>BST</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>..</span><span class='hs-layout'>)</span>
<span class=hs-linenum>33: </span>
<span class=hs-linenum>34: </span>          <span class='hs-comment'>-- * BST: Functions </span>
<span class=hs-linenum>35: </span>       <span class='hs-layout'>,</span> <span class='hs-varid'>mem</span><span class='hs-layout'>,</span> <span class='hs-varid'>add</span><span class='hs-layout'>,</span> <span class='hs-varid'>delMin</span><span class='hs-layout'>,</span> <span class='hs-varid'>del</span><span class='hs-layout'>,</span> <span class='hs-varid'>bstSort</span><span class='hs-layout'>,</span> <span class='hs-varid'>toBST</span><span class='hs-layout'>,</span> <span class='hs-varid'>toIncList</span>
<span class=hs-linenum>36: </span>
<span class=hs-linenum>37: </span>          <span class='hs-comment'>-- * BST: Examples</span>
<span class=hs-linenum>38: </span>       <span class='hs-layout'>,</span> <span class='hs-varid'>okBST</span><span class='hs-layout'>,</span> <span class='hs-varid'>badBST</span> 
<span class=hs-linenum>39: </span>                                              
<span class=hs-linenum>40: </span>       <span class='hs-layout'>)</span>
<span class=hs-linenum>41: </span>      <span class='hs-keyword'>where</span>
<span class=hs-linenum>42: </span>
<span class=hs-linenum>43: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span>      <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>abs</span><span class='hs-layout'>,</span> <span class='hs-varid'>length</span><span class='hs-layout'>,</span> <span class='hs-varid'>min</span><span class='hs-layout'>)</span>
<span class=hs-linenum>44: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>List</span>    <span class='hs-layout'>(</span><span class='hs-varid'>foldl'</span><span class='hs-layout'>)</span>
<span class=hs-linenum>45: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Vector</span>  <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>singleton</span><span class='hs-layout'>,</span> <span class='hs-varid'>foldl'</span><span class='hs-layout'>,</span> <span class='hs-varid'>foldr</span><span class='hs-layout'>,</span> <span class='hs-varid'>fromList</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>46: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Maybe</span>   <span class='hs-layout'>(</span><span class='hs-varid'>fromJust</span><span class='hs-layout'>)</span>
<span class=hs-linenum>47: </span>
<span class=hs-linenum>48: </span><span class='hs-definition'>dotProd</span><span class='hs-layout'>,</span> <span class='hs-varid'>dotProd'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Vector</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>49: </span><span class='hs-definition'>test1</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>50: </span><span class='hs-definition'>test2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>51: </span>
<span class=hs-linenum>52: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>die</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| false}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>53: </span><a class=annot href="#"><span class=annottext>forall a. {v : [Char] | false} -&gt; a</span><span class='hs-definition'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; a</span><span class='hs-varid'>error</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a>
</pre>
\end{comment}

So far, we have seen how to refine the types of *functions*, to
specify, for example, preconditions on the inputs, or postconditions
on the outputs. Very often, we wish to define *datatypes* that satisfy
certain invariants. In these cases, it is handy to be able to directly
refine the the `data` definition, so that it is impossible to create
illegal inhabitants.

Sparse Vectors Revisited {#sparsedata}
-------------------------------------

As our first example of a refined datatype, lets revisit the
sparse vector representation that we [saw earlier](#sparsetype).
The `SparseN` type alias we used got the job done, but is not
pleasant to work with because we have no way of determining
the *dimension* of the sparse vector. Instead, lets create a new
datatype to represent such vectors:


<pre><span class=hs-linenum>75: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>SP</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a. (Sparse a) -&gt; Int</span><span class='hs-varid'>spDim</span></a>   <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>76: </span>                   <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (Sparse a) -&gt; [(Int, a)]</span><span class='hs-varid'>spElems</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-layout'>}</span> 
</pre>

\noindent
Thus, a sparse vector is a pair of a dimension and a list of
index-value tuples. Implicitly, all indices *other* than those
in the list have the value `0` or the equivalent value type `a`.

\newthought{Legal}
`Sparse` vectors satisfy two crucial properties.
First, the dimension stored in `spDim` is non-negative.
Second, every index in `spElems` must be valid, i.e.
between `0` and the dimension. Unfortunately, Haskell's
type system does not make it easy to ensure that
*illegal vectors are not representable*.
\footnotetext{The standard approach is to use abstract types and
[smart constructors](https://www.haskell.org/haskellwiki/Smart_constructors)
but even then there is only the informal guarantee that the
smart constructor establishes the right invariants.}

\newthought{Data Invariants} LiquidHaskell lets us enforce
these invariants with a refined data definition:


<pre><span class=hs-linenum>100: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>SP</span> <span class='hs-layout'>{</span> <span class='hs-varid'>spDim</span>   <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Nat</span> 
<span class=hs-linenum>101: </span>                       <span class='hs-layout'>,</span> <span class='hs-varid'>spElems</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Btwn</span> <span class='hs-num'>0</span> <span class='hs-varid'>spDim</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent Where, as before, the we use the aliases:


<pre><span class=hs-linenum>107: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Nat</span>        <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span>            <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>108: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Btwn</span> <span class='hs-conid'>Lo</span> <span class='hs-conid'>Hi</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Lo</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;</span> <span class='hs-conid'>Hi</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\newthought{Refined Data Constructors} The refined data
definition is internally converted into refined types
for the data constructor `SP`:


<pre><span class=hs-linenum>116: </span><span class='hs-comment'>-- Generated Internal representation</span>
<span class=hs-linenum>117: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>118: </span>  <span class='hs-conid'>SP</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>spDim</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>spElems</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Btwn</span> <span class='hs-num'>0</span> <span class='hs-varid'>spDim</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> 
</pre>

{#autosmart}
\noindent In other words, by using refined input types for `SP`
we have automatically converted it into a *smart* constructor that
ensures that *every* instance of a `Sparse` is legal.
Consequently, LiquidHaskell verifies:


<pre><span class=hs-linenum>128: </span><span class='hs-definition'>okSP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>129: </span><a class=annot href="#"><span class=annottext>(Sparse [Char])</span><span class='hs-definition'>okSP</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:[({v : Int | v &lt; x1 &amp;&amp; 0 &lt;= v}, [Char])]
-&gt; {v : (Sparse [Char]) | spDim v == x1 &amp;&amp; spElems v == x2}</span><span class='hs-conid'>SP</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a> <span class='hs-keyglyph'>[</span> <a class=annot href="#"><span class=annottext>({v : Int | v == 0}, [Char])</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"cat"</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>130: </span>            <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>({v : Int | v &gt; 0}, [Char])</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"dog"</span></a><span class='hs-layout'>)</span> <span class='hs-keyglyph'>]</span> 
</pre>

\noindent but rejects, due to the invalid index:


<pre><span class=hs-linenum>136: </span><span class='hs-definition'>badSP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Sparse</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>137: </span><a class=annot href="#"><span class=annottext>(Sparse [Char])</span><span class='hs-definition'>badSP</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:[({v : Int | v &lt; x1 &amp;&amp; 0 &lt;= v}, [Char])]
-&gt; {v : (Sparse [Char]) | spDim v == x1 &amp;&amp; spElems v == x2}</span><span class='hs-conid'>SP</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a> <span class=hs-error><span class='hs-keyglyph'>[</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>({v : Int | v == 0}, [Char])</span><span class='hs-layout'>(</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"cat"</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error>
</span><span class=hs-linenum>138: </span>             <span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>({v : Int | v &gt; 0}, [Char])</span><span class='hs-layout'>(</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (6  :  int)}</span><span class='hs-num'>6</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"dog"</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error> </span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
</pre>

\newthought{Field Measures} It is convenient to write an alias
for sparse vectors of a given size `N`. We can use the field name
`spDim` as a *measure*, like `vlen`. That is, we can use `spDim`
inside refinements:


<pre><span class=hs-linenum>147: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>SparseN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>spDim</span> <span class='hs-varid'>v</span> <span class='hs-varop'>==</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span> 
</pre>

\newthought{Sparse Products}
Lets write a function to compute a sparse product


<pre><span class=hs-linenum>154: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>dotProd</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>SparseN</span> <span class='hs-conid'>Int</span> <span class='hs-layout'>(</span><span class='hs-varid'>vlen</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>155: </span><a class=annot href="#"><span class=annottext>x1:(Vector Int) -&gt; {v : (Sparse Int) | spDim v == vlen x1} -&gt; Int</span><span class='hs-definition'>dotProd</span></a> <a class=annot href="#"><span class=annottext>(Vector Int)</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>SP</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int
-&gt; {v : [({v : Int | v &gt;= 0}, Int)] | len v &gt;= 0 &amp;&amp; len v &lt;= len y}
-&gt; Int</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : [({v : Int | 0 &lt;= v}, Int)] | v == y &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>y</span></a>
<span class=hs-linenum>156: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>157: </span>    <a class=annot href="#"><span class=annottext>Int
-&gt; {v : [({v : Int | v &gt;= 0}, Int)] | len v &gt;= 0 &amp;&amp; len v &lt;= len y}
-&gt; Int</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>sum</span></a> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>y'</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int
-&gt; {VV : [({VV : Int | VV &gt;= 0}, Int)] | len VV &gt;= 0 &amp;&amp; len VV &lt;= len y}
-&gt; Int</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == sum}</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : (Vector Int) | v == x &amp;&amp; 0 &lt;= vlen v}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Vector Int) -&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt; vlen x1} -&gt; Int</span><span class='hs-varop'>!</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>i</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:Int
-&gt; {v : Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a> <a class=annot href="#"><span class=annottext>{fix#v#39# : Int | v' == v}</span><span class='hs-varid'>v</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [({v : Int | v &gt;= 0}, Int)] | v == y' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>y'</span></a> 
<span class=hs-linenum>158: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>sum</span> <span class='hs-conid'>[]</span>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == sum}</span><span class='hs-varid'>sum</span></a>
</pre>

LiquidHaskell verifies the above by using the specification
to conclude that for each tuple `(i, v)` in the list `y`, the
value of `i` is within the bounds of the vector `x`, thereby
proving `x ! i` safe.

\newthought{Folded Product} We can port the `fold`-based product
to our new representation:


<pre><span class=hs-linenum>170: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>dotProd'</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Vector</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>SparseN</span> <span class='hs-conid'>Int</span> <span class='hs-layout'>(</span><span class='hs-varid'>vlen</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>171: </span><a class=annot href="#"><span class=annottext>x1:(Vector Int) -&gt; {v : (Sparse Int) | spDim v == vlen x1} -&gt; Int</span><span class='hs-definition'>dotProd'</span></a> <a class=annot href="#"><span class=annottext>(Vector Int)</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>SP</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Int -&gt; ({v : Int | v &gt;= 0}, Int) -&gt; Int)
-&gt; Int -&gt; [({v : Int | v &gt;= 0}, Int)] -&gt; Int</span><span class='hs-varid'>foldl'</span></a> <a class=annot href="#"><span class=annottext>Int -&gt; ({v : Int | v &gt;= 0}, Int) -&gt; Int</span><span class='hs-varid'>body</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : [({v : Int | 0 &lt;= v}, Int)] | v == y &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>y</span></a>   
<span class=hs-linenum>172: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>173: </span>    <a class=annot href="#"><span class=annottext>Int -&gt; ({VV : Int | VV &gt;= 0}, Int) -&gt; Int</span><span class='hs-varid'>body</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>sum</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>i</span><span class='hs-layout'>,</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == sum}</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : (Vector Int) | v == x &amp;&amp; 0 &lt;= vlen v}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Vector Int) -&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt; vlen x1} -&gt; Int</span><span class='hs-varop'>!</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>i</span></a><span class='hs-layout'>)</span>  <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:Int
-&gt; {v : Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a> <a class=annot href="#"><span class=annottext>{fix#v#39# : Int | v' == v}</span><span class='hs-varid'>v</span></a>
</pre>

\noindent As before, LiquidHaskell checks the above by
[automatically instantiating refinements](#sparsetype)
for the type parameters of `foldl'`, saving us a fair
bit of typing and enabling the use of the elegant
polymorphic, higher-order combinators we know and love.

\exercise {} **[Sanitization]** Invariants are all well
and good for data computed *inside* our programs.
The only way to ensure the legality of data coming
from *outside*, i.e. from the "real world", is to
writing a sanitizer that will check the appropriate
invariants before constructing a `Sparse` vector.
Write the specification and implementation of a
sanitizer `fromList`, so that the following typechecks:


<pre><span class=hs-linenum>192: </span><span class='hs-definition'>fromList</span>          <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>   <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>193: </span><a class=annot href="#"><span class=annottext>forall a. Int -&gt; [(Int, a)] -&gt; (Maybe (Sparse a))</span><span class='hs-definition'>fromList</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>dim</span></a> <a class=annot href="#"><span class=annottext>[(Int, a)]</span><span class='hs-varid'>elts</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>   
<span class=hs-linenum>194: </span>
<span class=hs-linenum>195: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test1</span>         <span class='hs-keyglyph'>::</span> <span class='hs-conid'>SparseN</span> <span class='hs-conid'>String</span> <span class='hs-num'>3</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>196: </span><a class=annot href="#"><span class=annottext>{v : (Sparse [Char]) | spDim v == 3}</span><span class='hs-definition'>test1</span></a>             <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>(Maybe (Sparse [Char])) -&gt; (Sparse [Char])</span><span class='hs-varid'>fromJust</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>((Maybe (Sparse [Char])) -&gt; (Sparse [Char]))
-&gt; (Maybe (Sparse [Char])) -&gt; (Sparse [Char])</span><span class='hs-varop'>$</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>Int -&gt; [(Int, [Char])] -&gt; (Maybe (Sparse [Char]))</span><span class='hs-varid'>fromList</span></a></span><span class=hs-error>  </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [(Int, [Char])] | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>(Int, [Char])</span><span class='hs-layout'>(</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"cat"</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>(Int, [Char])</span><span class='hs-layout'>(</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"mouse"</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
</pre>

\exercise {} **[Addition]** Write the specification and implementation
of a function `plus` that performs the addition of two `Sparse`
vectors of the *same* dimension, yielding an output of that dimension.
When you are done, the following code should typecheck:


<pre><span class=hs-linenum>205: </span><span class='hs-definition'>plus</span>     <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Num</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Sparse</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>206: </span><a class=annot href="#"><span class=annottext>forall a. (Num a) =&gt; (Sparse a) -&gt; (Sparse a) -&gt; (Sparse a)</span><span class='hs-definition'>plus</span></a> <a class=annot href="#"><span class=annottext>(Sparse a)</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(Sparse a)</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a> 
<span class=hs-linenum>207: </span>
<span class=hs-linenum>208: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>SparseN</span> <span class='hs-conid'>Int</span> <span class='hs-num'>3</span> <span class='hs-keyword'>@-}</span>   
<span class=hs-linenum>209: </span><a class=annot href="#"><span class=annottext>{v : (Sparse Int) | spDim v == 3}</span><span class='hs-definition'>test2</span></a>    <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>(Sparse Int) -&gt; (Sparse Int) -&gt; (Sparse Int)</span><span class='hs-varid'>plus</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Sparse Int) | v == vec1}</span><span class='hs-varid'>vec1</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Sparse Int) | v == vec2}</span><span class='hs-varid'>vec2</span></a></span> 
<span class=hs-linenum>210: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>211: </span>    <a class=annot href="#"><span class=annottext>(Sparse Int)</span><span class='hs-varid'>vec1</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:[({v : Int | v &lt; x1 &amp;&amp; 0 &lt;= v}, Int)]
-&gt; {v : (Sparse Int) | spDim v == x1 &amp;&amp; spElems v == x2}</span><span class='hs-conid'>SP</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : [({v : Int | v == fst x15 &amp;&amp; v &gt;= 0}, Int)]&lt;\x6 VV -&gt; v /= x6&gt; | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>({v : Int | v == 0}, Int)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (12  :  int)}</span><span class='hs-num'>12</span></a><span class='hs-layout'>)</span><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>({v : Int | v &gt; 0}, Int)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (9  :  int)}</span><span class='hs-num'>9</span></a><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>212: </span>    <a class=annot href="#"><span class=annottext>(Sparse Int)</span><span class='hs-varid'>vec2</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:[({v : Int | v &lt; x1 &amp;&amp; 0 &lt;= v}, Int)]
-&gt; {v : (Sparse Int) | spDim v == x1 &amp;&amp; spElems v == x2}</span><span class='hs-conid'>SP</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : [({v : Int | v == fst x16 &amp;&amp; v &gt;= 0}, Int)]&lt;\x6 VV -&gt; v /= x6&gt; | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>({v : Int | v == 0}, Int)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (8  :  int)}</span><span class='hs-num'>8</span></a><span class='hs-layout'>)</span><span class='hs-layout'>,</span>  <a class=annot href="#"><span class=annottext>({v : Int | v == 1 &amp;&amp; v &gt; 0}, Int)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (100  :  int)}</span><span class='hs-num'>100</span></a><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
</pre>

Ordered Lists
--------------

As a second example of refined data types, lets consider a
different problem: representing *ordered* sequences. Here's
a type for sequences that mimics the classical list:


<pre><span class=hs-linenum>223: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Emp</span>
<span class=hs-linenum>224: </span>               <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-conop'>:&lt;</span><span class='hs-layout'>)</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a. (IncList a) -&gt; a</span><span class='hs-varid'>hd</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>tl</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-layout'>}</span>   
<span class=hs-linenum>225: </span>
<span class=hs-linenum>226: </span><span class='hs-keyword'>infixr</span> <span class='hs-num'>9</span> <span class='hs-conop'>:&lt;</span>
</pre>

\noindent 
The Haskell type above does not state that the elements
be in order of course, but we can specify that requirement
by refining *every* element in `tl` to be *greater than* `hd`: 


<pre><span class=hs-linenum>235: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Emp</span>
<span class=hs-linenum>236: </span>                   <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-conop'>:&lt;</span><span class='hs-layout'>)</span> <span class='hs-layout'>{</span> <span class='hs-varid'>hd</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>tl</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IncList</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>hd</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>237: </span>  <span class='hs-keyword'>@-}</span>
</pre>

\newthought{Refined Data Constructors} Once again,
the refined data definition is internally converted
into a "smart" refined data constructor


<pre><span class=hs-linenum>245: </span><span class='hs-comment'>-- Generated Internal representation</span>
<span class=hs-linenum>246: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>247: </span>  <span class='hs-conid'>Emp</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>248: </span>  <span class='hs-layout'>(</span><span class='hs-conop'>:&lt;</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>hd</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>tl</span><span class='hs-conop'>:</span><span class='hs-conid'>IncList</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>hd</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
</pre>

\noindent which ensures that we can only create legal ordered lists.


<pre><span class=hs-linenum>254: </span><a class=annot href="#"><span class=annottext>(IncList Integer)</span><span class='hs-definition'>okList</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Integer | v == 1}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Integer
-&gt; x2:(IncList {v : Integer | x1 &lt;= v})
-&gt; {v : (IncList Integer) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>(IncList {v : Integer | v &gt; 0})</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>x1:{v : Integer | v &gt; 0}
-&gt; x2:(IncList {v : Integer | v &gt; 0 &amp;&amp; x1 &lt;= v})
-&gt; {v : (IncList {v : Integer | v &gt; 0}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>(IncList {v : Integer | v &gt; 0})</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>x1:{v : Integer | v &gt; 0}
-&gt; x2:(IncList {v : Integer | v &gt; 0 &amp;&amp; x1 &lt;= v})
-&gt; {v : (IncList {v : Integer | v &gt; 0}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>(IncList {v : Integer | false})</span><span class='hs-conid'>Emp</span></a>      <span class='hs-comment'>-- accepted by LH</span>
<span class=hs-linenum>255: </span>
<span class=hs-linenum>256: </span><a class=annot href="#"><span class=annottext>(IncList Integer)</span><span class='hs-definition'>badList</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Integer | v == 2}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>x1:Integer
-&gt; x2:(IncList {v : Integer | x1 &lt;= v})
-&gt; {v : (IncList Integer) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>(IncList {v : Integer | v &gt; 0})</span><span class='hs-num'>1</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : Integer | v &gt; 0}
-&gt; x2:(IncList {v : Integer | v &gt; 0 &amp;&amp; x1 &lt;= v})
-&gt; {v : (IncList {v : Integer | v &gt; 0}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>(IncList {v : Integer | v &gt; 0})</span><span class='hs-num'>3</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : Integer | v &gt; 0}
-&gt; x2:(IncList {v : Integer | v &gt; 0 &amp;&amp; x1 &lt;= v})
-&gt; {v : (IncList {v : Integer | v &gt; 0}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>(IncList {v : Integer | false})</span><span class='hs-conid'>Emp</span></a></span>      <span class='hs-comment'>-- rejected by LH</span>
</pre>

\noindent 
Its all very well to *specify* ordered lists.
Next, lets see how its equally easy to *establish*
these invariants by implementing several textbook
sorting routines.

\newthought{Insertion Sort} 
First, lets implement insertion sort, which converts an ordinary
list `[a]` into an ordered list `IncList a`.


<pre><span class=hs-linenum>270: </span><span class='hs-definition'>insertSort</span>        <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> 
<span class=hs-linenum>271: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-definition'>insertSort</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (IncList a)</span><span class='hs-conid'>Emp</span></a> 
<span class=hs-linenum>272: </span><span class='hs-definition'>insertSort</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>insert</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-varid'>insertSort</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span> 
</pre>

The hard work is done by `insert` which places an element into
the correct position of a sorted list. LiquidHaskell infers that
if you give `insert` an element and a sorted list, it returns a
sorted list.


<pre><span class=hs-linenum>281: </span><span class='hs-definition'>insert</span>             <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>282: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-definition'>insert</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>y</span></a> <span class='hs-conid'>Emp</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV == y}
-&gt; x2:(IncList {VV : a | VV == y &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV == y}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>(IncList {VV : a | false})</span><span class='hs-conid'>Emp</span></a>
<span class=hs-linenum>283: </span><span class='hs-definition'>insert</span> <span class='hs-varid'>y</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-conop'>:&lt;</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>284: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt;= y}
-&gt; x2:(IncList {VV : a | VV &gt;= y &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt;= y}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt;= y &amp;&amp; VV &gt;= x}
-&gt; x2:(IncList {VV : a | VV &gt;= y &amp;&amp; VV &gt;= x &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt;= y &amp;&amp; VV &gt;= x}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | x &lt;= VV}) | v == xs}</span><span class='hs-varid'>xs</span></a> 
<span class=hs-linenum>285: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt;= x}
-&gt; x2:(IncList {VV : a | VV &gt;= x &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt;= x}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>insert</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | x &lt;= VV}) | v == xs}</span><span class='hs-varid'>xs</span></a>
</pre>

\exercise Complete the implementation of the function below to
use `foldr` to eliminate the explicit recursion in `insertSort`.


<pre><span class=hs-linenum>292: </span><span class='hs-definition'>insertSort'</span>     <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>293: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-definition'>insertSort'</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(a -&gt; (IncList a) -&gt; (IncList a))
-&gt; (IncList a) -&gt; [a] -&gt; (IncList a)</span><span class='hs-varid'>foldr</span></a> <a class=annot href="#"><span class=annottext>a -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>(IncList a)</span><span class='hs-varid'>b</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>294: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>295: </span>     <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>f</span></a>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>undefined</span></a>    <span class='hs-comment'>-- Fill this in</span>
<span class=hs-linenum>296: </span>     <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>b</span></a>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>undefined</span></a>    <span class='hs-comment'>-- Fill this in</span>
</pre>

\newthought{Merge Sort} Similarly, it is easy to write merge sort,
by implementing the three steps. First, we write a function that
*splits* the input into two equal sized halves:


<pre><span class=hs-linenum>304: </span><span class='hs-definition'>split</span>          <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span>
<span class=hs-linenum>305: </span><a class=annot href="#"><span class=annottext>forall a.
x1:{VV : [a] | len VV &gt;= 0}
-&gt; ({VV : [a] | VV == fst VV &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1}, {VV : [a] | VV == snd VV &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1})&lt;\x1 VV -&gt; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1 &amp;&amp; len VV &lt;= len x1&gt;</span><span class='hs-definition'>split</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>zs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x6 VV -&gt; p2 x6&gt; | fst v == x1 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; snd v == x2 &amp;&amp; x_Tuple21 v == x1}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; v == xs &amp;&amp; len v == len xs &amp;&amp; len v &gt;= 0 &amp;&amp; len v &gt;= len ys &amp;&amp; len v &lt;= len zs}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len xs &amp;&amp; len v &lt;= len xs &amp;&amp; len v &lt;= len zs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> 
<span class=hs-linenum>306: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>307: </span>    <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == xs &amp;&amp; len VV == len xs &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &gt;= len ys &amp;&amp; len VV &lt;= len zs}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len xs &amp;&amp; len VV &lt;= len xs &amp;&amp; len VV &lt;= len zs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
x1:{VV : [a] | len VV &gt;= 0}
-&gt; ({VV : [a] | VV == fst VV &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1}, {VV : [a] | VV == snd VV &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1})&lt;\x1 VV -&gt; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1 &amp;&amp; len VV &lt;= len x1&gt;</span><span class='hs-varid'>split</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs</span></a>
<span class=hs-linenum>308: </span><span class='hs-definition'>split</span> <span class='hs-varid'>xs</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x6 VV -&gt; p2 x6&gt; | fst v == x1 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; snd v == x2 &amp;&amp; x_Tuple21 v == x1}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [{VV : a | false}] | null v &lt;=&gt; true &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a><span class='hs-layout'>)</span>
</pre>

\noindent 
Second, we need a function that *combines* two ordered lists


<pre><span class=hs-linenum>315: </span><span class='hs-definition'>merge</span>         <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> 
<span class=hs-linenum>316: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-definition'>merge</span></a> <a class=annot href="#"><span class=annottext>(IncList a)</span><span class='hs-varid'>xs</span></a>  <span class='hs-conid'>Emp</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (IncList a) | v == xs}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>317: </span><span class='hs-definition'>merge</span> <span class='hs-conid'>Emp</span> <span class='hs-varid'>ys</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IncList a)</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>318: </span><span class='hs-definition'>merge</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-conop'>:&lt;</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>y</span> <span class='hs-conop'>:&lt;</span> <span class='hs-varid'>ys</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>319: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt;= x}
-&gt; x2:(IncList {VV : a | VV &gt;= x &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt;= x}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>merge</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | x &lt;= VV}) | v == xs}</span><span class='hs-varid'>xs</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt;= y &amp;&amp; VV &gt;= x}
-&gt; x2:(IncList {VV : a | VV &gt;= y &amp;&amp; VV &gt;= x &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt;= y &amp;&amp; VV &gt;= x}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | y &lt;= VV}) | v == ys}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>320: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt;= y}
-&gt; x2:(IncList {VV : a | VV &gt;= y &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt;= y}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>merge</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt; y &amp;&amp; VV &gt;= x}
-&gt; x2:(IncList {VV : a | VV &gt; y &amp;&amp; VV &gt;= x &amp;&amp; x1 &lt;= VV})
-&gt; {v : (IncList {VV : a | VV &gt; y &amp;&amp; VV &gt;= x}) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | x &lt;= VV}) | v == xs}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | y &lt;= VV}) | v == ys}</span><span class='hs-varid'>ys</span></a>
</pre>

\noindent 
Finally, we compose the above steps to divide (i.e. `split`)
and conquer (`sort` and `merge`) the input list:


<pre><span class=hs-linenum>328: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>mergeSort</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>329: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-definition'>mergeSort</span></a> <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (IncList a)</span><span class='hs-conid'>Emp</span></a>  
<span class=hs-linenum>330: </span><span class='hs-definition'>mergeSort</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>x</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(IncList {VV : a | x1 &lt;= VV})
-&gt; {v : (IncList a) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <a class=annot href="#"><span class=annottext>(IncList {VV : a | false})</span><span class='hs-conid'>Emp</span></a>
<span class=hs-linenum>331: </span><span class='hs-definition'>mergeSort</span> <span class='hs-varid'>xs</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>merge</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-varid'>mergeSort</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0 &amp;&amp; len v &gt;= len zs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-varid'>mergeSort</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len ys &amp;&amp; len v &lt;= len ys}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span> 
<span class=hs-linenum>332: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>333: </span>    <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &gt;= len zs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : [a] | VV == zs &amp;&amp; len VV == len zs &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len ys &amp;&amp; len VV &lt;= len ys}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
x1:{VV : [a] | len VV &gt;= 0}
-&gt; ({VV : [a] | VV == fst VV &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1}, {VV : [a] | VV == snd VV &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1})&lt;\x1 VV -&gt; len VV &gt;= 0 &amp;&amp; len VV &lt;= len x1 &amp;&amp; len VV &lt;= len x1&gt;</span><span class='hs-varid'>split</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\exercise Why is the following implementation of `quickSort`
rejected by LiquidHaskell? Modify it so it is accepted.


<pre><span class=hs-linenum>340: </span><span class='hs-definition'>quickSort</span>           <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>341: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-definition'>quickSort</span></a> <span class='hs-conid'>[]</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (IncList a)</span><span class='hs-conid'>Emp</span></a> 
<span class=hs-linenum>342: </span><span class='hs-definition'>quickSort</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>append</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | VV &lt; x}) | v == lessers}</span><span class='hs-varid'>lessers</span></a> <a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | VV &gt;= x}) | v == greaters}</span><span class='hs-varid'>greaters</span></a> 
<span class=hs-linenum>343: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>344: </span>    <a class=annot href="#"><span class=annottext>(IncList {VV : a | VV &lt; x})</span><span class='hs-varid'>lessers</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-varid'>quickSort</span></a> <a class=annot href="#"><span class=annottext>{v : [{VV : a | VV &lt; x}] | len v &gt;= 0 &amp;&amp; len v &lt;= len xs}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>y</span> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>]</span>
<span class=hs-linenum>345: </span>    <a class=annot href="#"><span class=annottext>(IncList {VV : a | VV &gt;= x})</span><span class='hs-varid'>greaters</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-varid'>quickSort</span></a> <a class=annot href="#"><span class=annottext>{v : [{VV : a | VV &gt;= x}] | len v &gt;= 0 &amp;&amp; len v &lt;= len xs}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>z</span></a> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>z</span> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>z</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt;= v}</span><span class='hs-varop'>&gt;=</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>346: </span>
<span class=hs-linenum>347: </span><span class='hs-definition'>append</span>              <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>348: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-definition'>append</span></a> <span class='hs-conid'>Emp</span>       <a class=annot href="#"><span class=annottext>(IncList a)</span><span class='hs-varid'>ys</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (IncList a) | v == ys}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>349: </span><span class='hs-definition'>append</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span> <span class='hs-conop'>:&lt;</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varid'>ys</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(IncList {VV : a | x1 &lt;= VV})
-&gt; {v : (IncList a) | hd v == x1 &amp;&amp; tl v == x2}</span><span class='hs-conop'>:&lt;</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (IncList a) -&gt; (IncList a) -&gt; (IncList a)</span><span class='hs-varid'>append</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (IncList {VV : a | x &lt;= VV}) | v == xs}</span><span class='hs-varid'>xs</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (IncList a) | v == ys}</span><span class='hs-varid'>ys</span></a></span> 
</pre>

Ordered Trees {#binarysearchtree}
---------------------------------  

As a last example of refined data types, let us consider binary search ordered
trees, defined thus:


<pre><span class=hs-linenum>359: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Leaf</span>
<span class=hs-linenum>360: </span>           <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Node</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a. (BST a) -&gt; a</span><span class='hs-varid'>root</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>361: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (BST a) -&gt; (BST a)</span><span class='hs-varid'>left</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>362: </span>                  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (BST a) -&gt; (BST a)</span><span class='hs-varid'>right</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-layout'>}</span>
</pre>

\newthought{Binary Search Trees}
enjoy the [property][bst-wiki]
that each `root` lies (strictly) between the elements belonging in the
`left` and `right` subtrees hanging off the the root. The ordering
invariant makes it easy to check whether a certain value occurs in the
tree.  If the tree is empty i.e. a Leaf, then the value does not occur
in the tree.  If the given value is at the root then the value does
occur in the tree.  If it is less than (respectively greater than) the
root, we recursively check whether the value occurs in the left
(respectively right) subtree.


\begin{marginfigure}
\includegraphics[height=2.0in]{img/bst.png}
\caption{A Binary Search Tree with values between 1 and 9.
         Each root's value lies between the values appearing
         in its left and right subtrees.}
\label{fig:bst}
\end{marginfigure}


Figure \ref{fig:bst} shows a binary search tree whose nodes
are labeled with a subset of values from `1` to `9`.
We might represent such a tree with the Haskell value:


<pre><span class=hs-linenum>391: </span><span class='hs-definition'>okBST</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>392: </span><a class=annot href="#"><span class=annottext>(BST Int)</span><span class='hs-definition'>okBST</span></a> <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:(BST {v : Int | v &lt; x1})
-&gt; x3:(BST {v : Int | x1 &lt; v})
-&gt; {v : (BST Int) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (6  :  int)}</span><span class='hs-num'>6</span></a> 
<span class=hs-linenum>393: </span>             <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a>
<span class=hs-linenum>394: </span>                 <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v == 1 &amp;&amp; v &gt; 0}
-&gt; x2:(BST {v : Int | v == 1 &amp;&amp; v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v == 1 &amp;&amp; v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v == 1 &amp;&amp; v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>395: </span>                 <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
<span class=hs-linenum>396: </span>             <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (9  :  int)}</span><span class='hs-num'>9</span></a>
<span class=hs-linenum>397: </span>                 <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (7  :  int)}</span><span class='hs-num'>7</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>398: </span>                 <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{Refined Data Type} The Haskell type says nothing about the
ordering invariant, and hence, cannot prevent us from creating illegal
`BST` values that violate the invariant. We can remedy this with a
refined data definition that captures the invariant:


<pre><span class=hs-linenum>407: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Leaf</span>
<span class=hs-linenum>408: </span>               <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Node</span> <span class='hs-layout'>{</span> <span class='hs-varid'>root</span>  <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>409: </span>                      <span class='hs-layout'>,</span> <span class='hs-varid'>left</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>root</span><span class='hs-layout'>}</span>
<span class=hs-linenum>410: </span>                      <span class='hs-layout'>,</span> <span class='hs-varid'>right</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>root</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>411: </span>  <span class='hs-keyword'>@-}</span>
</pre>


\newthought{Refined Data Constructors} As before, the above data definition
creates a refined "smart" constructor for `BST`


<pre><span class=hs-linenum>419: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>420: </span>  <span class='hs-conid'>Leaf</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>421: </span>  <span class='hs-conid'>Node</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>r</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>r</span><span class='hs-layout'>}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>r</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
</pre>

\noindent which *prevents* us from creating illegal trees


<pre><span class=hs-linenum>427: </span><span class='hs-definition'>badBST</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>428: </span><a class=annot href="#"><span class=annottext>(BST Int)</span><span class='hs-definition'>badBST</span></a> <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:(BST {v : Int | v &lt; x1})
-&gt; x3:(BST {v : Int | x1 &lt; v})
-&gt; {v : (BST Int) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (6  :  int)}</span><span class='hs-num'>6</span></a> 
<span class=hs-linenum>429: </span>             <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a>
<span class=hs-linenum>430: </span>                 <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v == 1 &amp;&amp; v &gt; 0}
-&gt; x2:(BST {v : Int | v == 1 &amp;&amp; v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v == 1 &amp;&amp; v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v == 1 &amp;&amp; v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>431: </span>                 <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a></span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>    <span class='hs-comment'>-- Out of order, rejected by LH </span>
<span class=hs-linenum>432: </span>             <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (9  :  int)}</span><span class='hs-num'>9</span></a>
<span class=hs-linenum>433: </span>                 <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt; 0}
-&gt; x2:(BST {v : Int | v &gt; 0 &amp;&amp; v &lt; x1})
-&gt; x3:(BST {v : Int | v &gt; 0 &amp;&amp; x1 &lt; v})
-&gt; {v : (BST {v : Int | v &gt; 0}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (7  :  int)}</span><span class='hs-num'>7</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a> <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>434: </span>                 <a class=annot href="#"><span class=annottext>(BST {v : Int | false})</span><span class='hs-conid'>Leaf</span></a><span class='hs-layout'>)</span>
</pre>

\exercise Can a `BST Int` contain duplicates?

\newthought{Membership}
Lets write some functions to create and manipulate
these trees. First, a function to check whether a value
is in a `BST`:


<pre><span class=hs-linenum>445: </span><span class='hs-definition'>mem</span>                 <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>446: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; Bool</span><span class='hs-definition'>mem</span></a> <span class='hs-keyword'>_</span> <span class='hs-conid'>Leaf</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
<span class=hs-linenum>447: </span><span class='hs-definition'>mem</span> <span class='hs-varid'>k</span> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k'</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>448: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>
<span class=hs-linenum>449: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a>  <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; Bool</span><span class='hs-varid'>mem</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | VV &lt; k'}) | v == l}</span><span class='hs-varid'>l</span></a>
<span class=hs-linenum>450: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; Bool</span><span class='hs-varid'>mem</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | k' &lt; VV}) | v == r}</span><span class='hs-varid'>r</span></a> 
</pre>

\newthought{Singleton} Next, another easy warm-up: a function to create
a `BST` with a single given element:


<pre><span class=hs-linenum>457: </span><span class='hs-definition'>one</span>   <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>458: </span><a class=annot href="#"><span class=annottext>forall a. a -&gt; (BST a)</span><span class='hs-definition'>one</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(BST {VV : a | VV &lt; x1})
-&gt; x3:(BST {VV : a | x1 &lt; VV})
-&gt; {v : (BST a) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(BST {VV : a | false})</span><span class='hs-conid'>Leaf</span></a> <a class=annot href="#"><span class=annottext>(BST {VV : a | false})</span><span class='hs-conid'>Leaf</span></a>
</pre>

\newthought{Insertion} Next, lets write a function that adds an
element to a `BST`.
\footnotetext{Amusingly, while typing out the below I swapped the
`k` and `k'` which caused LiquidHaskell to complain.}


<pre><span class=hs-linenum>467: </span><span class='hs-definition'>add</span>                  <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>468: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; (BST a)</span><span class='hs-definition'>add</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k'</span></a> <span class='hs-conid'>Leaf</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == fix} -&gt; (BST {VV : a | VV == fix})</span><span class='hs-varid'>one</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a>
<span class=hs-linenum>469: </span><span class='hs-definition'>add</span> <span class='hs-varid'>k'</span> <span class='hs-varid'>t</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>470: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(BST {VV : a | VV &lt; x1})
-&gt; x3:(BST {VV : a | x1 &lt; VV})
-&gt; {v : (BST a) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; (BST a)</span><span class='hs-varid'>add</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | VV &lt; k}) | v == l}</span><span class='hs-varid'>l</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | k &lt; VV}) | v == r}</span><span class='hs-varid'>r</span></a>
<span class=hs-linenum>471: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>  <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(BST {VV : a | VV &lt; x1})
-&gt; x3:(BST {VV : a | x1 &lt; VV})
-&gt; {v : (BST a) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | VV &lt; k}) | v == l}</span><span class='hs-varid'>l</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; (BST a)</span><span class='hs-varid'>add</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | k &lt; VV}) | v == r}</span><span class='hs-varid'>r</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>472: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(BST a)</span><span class='hs-varid'>t</span></a> 
</pre>

\newthought{Minimum} Next, lets write a function to delete the *minimum*
element from a `BST`. This function will return a *pair* of outputs --
the smallest element and the remainder of the tree. We can say that the
output element is indeed the smallest, by saying that the remainder's
elements exceed the element. To this end, lets define a helper type:
\footnotetext{This helper type approach is rather verbose. We should be able
to just use plain old pairs and specify the above requirement as a
*dependency* between the pairs' elements. Later, we will see how to
do so using [abstract refinements](http://goto.ucsd.edu/~rjhala/liquid/abstract_refinement_types.pdf).}


<pre><span class=hs-linenum>486: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>MinPair</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>MP</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a. (MinPair a) -&gt; a</span><span class='hs-varid'>minElt</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (MinPair a) -&gt; (BST a)</span><span class='hs-varid'>rest</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-layout'>}</span>
</pre>

\noindent We can specify that `minElt` is indeed smaller than all
the elements in `rest` via the data type refinement:


<pre><span class=hs-linenum>493: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>MinPair</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>MP</span> <span class='hs-layout'>{</span> <span class='hs-varid'>minElt</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>rest</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>minElt</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent Finally, we can write the code to compute `MinPair`


<pre><span class=hs-linenum>499: </span><span class='hs-definition'>delMin</span>                 <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>MinPair</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>500: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (BST a) -&gt; (MinPair a)</span><span class='hs-definition'>delMin</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-conid'>Leaf</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(BST {VV : a | x1 &lt; VV})
-&gt; {v : (MinPair a) | minElt v == x1 &amp;&amp; rest v == x2}</span><span class='hs-conid'>MP</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | k &lt; VV}) | v == r}</span><span class='hs-varid'>r</span></a>
<span class=hs-linenum>501: </span><span class='hs-definition'>delMin</span> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(BST {VV : a | x1 &lt; VV})
-&gt; {v : (MinPair a) | minElt v == x1 &amp;&amp; rest v == x2}</span><span class='hs-conid'>MP</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k' &amp;&amp; VV == fix &amp;&amp; VV &lt; k}</span><span class='hs-varid'>k'</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{VV : a | VV &gt; fix &amp;&amp; VV &gt; fix}
-&gt; x2:(BST {VV : a | VV &gt; fix &amp;&amp; VV &gt; fix &amp;&amp; VV &lt; x1})
-&gt; x3:(BST {VV : a | VV &gt; fix &amp;&amp; VV &gt; fix &amp;&amp; x1 &lt; VV})
-&gt; {v : (BST {VV : a | VV &gt; fix &amp;&amp; VV &gt; fix}) | left v == x2 &amp;&amp; root v == x1 &amp;&amp; right v == x3}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | VV &gt; fix &amp;&amp; VV &gt; fix &amp;&amp; VV &lt; k}) | v == l' &amp;&amp; v == fix}</span><span class='hs-varid'>l'</span></a> <a class=annot href="#"><span class=annottext>{v : (BST {VV : a | k &lt; VV}) | v == r}</span><span class='hs-varid'>r</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>502: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>503: </span>    <span class='hs-conid'>MP</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == fix &amp;&amp; VV &lt; k}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{VV : (BST {VV : a | VV &gt; fix &amp;&amp; VV &gt; fix &amp;&amp; VV &lt; k}) | VV == fix}</span><span class='hs-varid'>l'</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; (BST a) -&gt; (MinPair a)</span><span class='hs-varid'>delMin</span></a> <a class=annot href="#"><span class=annottext>(BST {VV : a | VV &lt; k})</span><span class='hs-varid'>l</span></a> 
<span class=hs-linenum>504: </span><span class='hs-definition'>delMin</span> <span class='hs-conid'>Leaf</span>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; (MinPair a)</span><span class='hs-varid'>die</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Don't say I didn't say I didn't warn ya!"</span></a></span>
</pre>

\exercise {} **[Deletion]** Use `delMin` to complete the implementation
of `del` which *deletes* a given element from a `BST`,
if it is present.


<pre><span class=hs-linenum>512: </span><span class='hs-definition'>del</span>                   <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>513: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; a -&gt; (BST a) -&gt; (BST a)</span><span class='hs-definition'>del</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>(BST a)</span><span class='hs-varid'>t</span></a><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>
<span class=hs-linenum>514: </span><span class='hs-definition'>del</span> <span class='hs-keyword'>_</span>  <span class='hs-varid'>t</span>              <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (BST a) | v == t}</span><span class='hs-varid'>t</span></a>
</pre>

\exercise The function `delMin` is only sensible for non-empty trees.
[Read ahead](#usingmeasures) to learn how to specify and verify that
it is only called with such trees, and then apply that technique here
to verify the call to `die` in `delMin`.

\exercise Complete the implementation of `toIncList` to obtain a `BST`
based sorting routine `bstSort`.


<pre><span class=hs-linenum>526: </span><span class='hs-definition'>bstSort</span>   <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>527: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (IncList a)</span><span class='hs-definition'>bstSort</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(BST a) -&gt; (IncList a)</span><span class='hs-varid'>toIncList</span></a> <a class=annot href="#"><span class=annottext>((BST a) -&gt; (IncList a))
-&gt; ([a] -&gt; (BST a)) -&gt; [a] -&gt; exists [(BST a)].(IncList a)</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>[a] -&gt; (BST a)</span><span class='hs-varid'>toBST</span></a>
<span class=hs-linenum>528: </span>
<span class=hs-linenum>529: </span><span class='hs-definition'>toBST</span>     <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>530: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; (BST a)</span><span class='hs-definition'>toBST</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(a -&gt; (BST a) -&gt; (BST a)) -&gt; (BST a) -&gt; [a] -&gt; (BST a)</span><span class='hs-varid'>foldr</span></a> <a class=annot href="#"><span class=annottext>a -&gt; (BST a) -&gt; (BST a)</span><span class='hs-varid'>add</span></a> <a class=annot href="#"><span class=annottext>(BST a)</span><span class='hs-conid'>Leaf</span></a>  
<span class=hs-linenum>531: </span>
<span class=hs-linenum>532: </span><span class='hs-definition'>toIncList</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IncList</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>533: </span><a class=annot href="#"><span class=annottext>forall a. (BST a) -&gt; (IncList a)</span><span class='hs-definition'>toIncList</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>
</pre>

\hint This exercise will be a lot easier after you finish the
`quickSort` exercise. Note that the signature for `toIncList`
does not use `Ord` and so you cannot use a sorting procedure
to implement it.


Recap
-----

In this chapter we saw how LiquidHaskell lets you refine data
type definitions to capture sophisticated invariants. These
definitions are internally represented by refining the types
of the data constructors, automatically making them "smart"  in
that they preclude the creation of illegal values that violate
the invariants. We will see much more of this handy technique
in future chapters.

One recurring theme in this chapter was that we had to create new
versions of standard datatypes, just in order to specify certain
invariants.  For example, we had to write a special list type, with
its own *copies* of nil and cons. Similarly, to implement `delMin` we
had to create our own pair type.

\newthought{This duplication} of types is quite tedious.
There should be a way to just slap the desired invariants
on to *existing* types, thereby facilitating their reuse.
In a few chapters, we will see how to achieve this reuse
by *abstracting refinements* from the definitions of
datatypes or functions in the same way we abstract
the element type `a` from containers like `[a]` or `BST a`.   




