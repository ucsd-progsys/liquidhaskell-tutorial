Boolean Measures {#boolmeasures}
================


In the last two chapters, we saw how refinements could be used to
reason about the properties of basic `Int` values like vector
indices, or the elements of a list. Next, lets see how we can
describe properties of aggregate structures like lists and trees,
and use these properties to improve the APIs for operating over
such structures.

\begin{comment}

<pre><span class=hs-linenum>14: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>15: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>16: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>17: </span>
<span class=hs-linenum>18: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>Measures</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>19: </span>
<span class=hs-linenum>20: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span> <span class='hs-varid'>hiding</span><span class='hs-layout'>(</span><span class='hs-varid'>foldr</span><span class='hs-layout'>,</span> <span class='hs-varid'>foldr1</span><span class='hs-layout'>,</span> <span class='hs-varid'>map</span><span class='hs-layout'>,</span> <span class='hs-varid'>sum</span><span class='hs-layout'>,</span> <span class='hs-varid'>head</span><span class='hs-layout'>,</span> <span class='hs-varid'>tail</span><span class='hs-layout'>,</span> <span class='hs-varid'>null</span><span class='hs-layout'>)</span>
<span class=hs-linenum>21: </span>
<span class=hs-linenum>22: </span><a class=annot href="#"><span class=annottext>(IO ())</span><span class='hs-definition'>main</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; (IO ())</span><span class='hs-varid'>putStrLn</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Hello"</span></a>
<span class=hs-linenum>23: </span>
<span class=hs-linenum>24: </span><span class='hs-comment'>-- | Old Definitions</span>
<span class=hs-linenum>25: </span>
<span class=hs-linenum>26: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Nat</span>     <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>27: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Pos</span>     <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;</span>  <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>28: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>NonZero</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>/=</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>29: </span>
<span class=hs-linenum>30: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>die</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| false}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>31: </span><a class=annot href="#"><span class=annottext>forall a. {v : [Char] | false} -&gt; a</span><span class='hs-definition'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; a</span><span class='hs-varid'>error</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a>
<span class=hs-linenum>32: </span>
<span class=hs-linenum>33: </span><span class='hs-comment'>-- Type Definitions</span>
<span class=hs-linenum>34: </span><span class='hs-definition'>divide</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>35: </span><span class='hs-definition'>size1</span><span class='hs-layout'>,</span> <span class='hs-varid'>size2</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
</pre>
\end{comment}


Partial Functions 
------------------

As a motivating example, let us return to problem of ensuring
the safety of division. Recall that we wrote:


<pre><span class=hs-linenum>47: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>divide</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>NonZero</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>48: </span><a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-definition'>divide</span></a> <span class='hs-keyword'>_</span> <span class='hs-num'>0</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; Int</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"divide-by-zero"</span></a>
<span class=hs-linenum>49: </span><span class='hs-definition'>divide</span> <span class='hs-varid'>x</span> <span class='hs-varid'>n</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:{v : Int | v /= 0}
-&gt; {v : Int | x1 &gt;= 0 &amp;&amp; x2 &gt;= 0 =&gt; v &gt;= 0 &amp;&amp; x1 &gt;= 0 &amp;&amp; x2 &gt;= 1 =&gt; v &lt;= x1 &amp;&amp; v == x1 / x2}</span><span class='hs-varop'>`div`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | 0 /= v}</span><span class='hs-varid'>n</span></a>
</pre>

\newthought{The Precondition} asserted by the input type `NonZero`
allows LiquidHaskell to prove that the `die` is *never* executed at
run-time, but consequently, requires us to establish that wherever
`divide` is *used*, the second parameter be provably non-zero.
This is requirement is not onerous when we know exactly what the
divisor is *statically*


<pre><span class=hs-linenum>60: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; Int</span><span class='hs-definition'>avg2</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>y</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-varid'>divide</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span>     <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a>
<span class=hs-linenum>61: </span>
<span class=hs-linenum>62: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; Int -&gt; Int</span><span class='hs-definition'>avg3</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>z</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-varid'>divide</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == z}</span><span class='hs-varid'>z</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a>
</pre>

\noindent However, it can be more of a challenge when the divisor
is obtained *dynamically*. For example, lets write a function to
find the number of elements in a list


<pre><span class=hs-linenum>70: </span><span class='hs-definition'>size</span>        <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>71: </span><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-definition'>size</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>72: </span><span class='hs-definition'>size</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\noindent and use it to compute the average value of a list:


<pre><span class=hs-linenum>78: </span><a class=annot href="#"><span class=annottext>[Int] -&gt; Int</span><span class='hs-definition'>avgMany</span></a> <a class=annot href="#"><span class=annottext>[Int]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-varid'>divide</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == total}</span><span class='hs-varid'>total</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | notEmpty xs =&gt; v &gt; 0 &amp;&amp; v == elems &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>elems</span></a></span> 
<span class=hs-linenum>79: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>80: </span>    <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>total</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v} -&gt; Int</span><span class='hs-varid'>sum</span></a>  <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Int] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>
<span class=hs-linenum>81: </span>    <a class=annot href="#"><span class=annottext>{v : Int | notEmpty xs =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>elems</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

Uh oh. LiquidHaskell wags its finger at us! 

\begin{liquiderror}
     src/04-measure.lhs:77:27-31: Error: Liquid Type Mismatch
       Inferred type
         VV : Int | VV == elems
      
       not a subtype of Required type
         VV : Int | 0 /= VV
      
       In Context
         VV    : Int | VV == elems
         elems : Int
\end{liquiderror}

\newthought{We cannot prove} that the divisor is `NonZero`,
because it *can be* `0` -- when the list is *empty*. Thus, we
need a way of specifying that the input to `avgMany` is indeed
non-empty!

Lifting Functions to Measures {#usingmeasures}
-----------------------------

\newthought{How} shall we tell LiquidHaskell that a list is *non-empty*?
Recall the notion of `measure` previously [introduced](#vectorbounds)
to describe the size of a `Data.Vector`. In that spirit, lets write
a function that computes whether a list is not empty:


<pre><span class=hs-linenum>113: </span><span class='hs-definition'>notEmpty</span>       <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span> 
<span class=hs-linenum>114: </span><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {VV : Bool | Prop VV &lt;=&gt; notEmpty x1}</span><span class='hs-definition'>notEmpty</span></a> <span class='hs-conid'>[]</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
<span class=hs-linenum>115: </span><span class='hs-definition'>notEmpty</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a> 
</pre>

\newthought{A measure} is a *total* Haskell function,

1. With a *single* equation per data constructor, and 
2. Guaranteed to *terminate*, typically via structural recursion.

\noindent
We can tell LiquidHaskell to *lift* a function meeting
the above requirements into the refinement logic by declaring:


<pre><span class=hs-linenum>128: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>notEmpty</span> <span class='hs-keyword'>@-}</span>
</pre>



\newthought{Non-Empty Lists}
To use the newly defined measure, we define an alias for 
non-empty lists, i.e. the *subset* of plain old Haskell
lists `[a]` for which the predicate `notEmpty` holds
                

<pre><span class=hs-linenum>139: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>notEmpty</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>


We can now refine various signatures to establish the safety of
the list-average function.

\newthought{Size} First, we specify that `size` returns
a non-zero value when the input list is not-empty:


<pre><span class=hs-linenum>150: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>size</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Nat</span> <span class='hs-keyword'>| notEmpty xs =&gt; v &gt; 0}</span> <span class='hs-keyword'>@-}</span>
</pre>

\newthought{Average} Second, we specify that the `average`
is only sensible for non-empty lists:


<pre><span class=hs-linenum>157: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>average</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>NEList</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>158: </span><a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v} -&gt; Int</span><span class='hs-definition'>average</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v}</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-varid'>divide</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == total}</span><span class='hs-varid'>total</span></a> <a class=annot href="#"><span class=annottext>{v : Int | notEmpty xs =&gt; v &gt; 0 &amp;&amp; v == elems &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>elems</span></a>
<span class=hs-linenum>159: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>160: </span>    <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>total</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v} -&gt; Int</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v &amp;&amp; v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>161: </span>    <a class=annot href="#"><span class=annottext>{v : Int | notEmpty xs =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>elems</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v &amp;&amp; v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\exercise Fix the code below to obtain an alternate variant
`average'` that returns `Nothing` for empty lists:


<pre><span class=hs-linenum>168: </span><span class='hs-definition'>average'</span>      <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>169: </span><a class=annot href="#"><span class=annottext>[Int] -&gt; (Maybe Int)</span><span class='hs-definition'>average'</span></a> <a class=annot href="#"><span class=annottext>[Int]</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>170: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True &amp;&amp; v == ok}</span><span class='hs-varid'>ok</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int -&gt; {v : (Maybe Int) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <a class=annot href="#"><span class=annottext>(Int -&gt; (Maybe Int)) -&gt; Int -&gt; (Maybe Int)</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-varid'>divide</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == total}</span><span class='hs-varid'>total</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | notEmpty xs =&gt; v &gt; 0 &amp;&amp; v == elems &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>elems</span></a></span> 
<span class=hs-linenum>171: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (Maybe Int) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a> 
<span class=hs-linenum>172: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>173: </span>    <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>total</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Int] | notEmpty v} -&gt; Int</span><span class='hs-varid'>sum</span></a>  <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Int] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>
<span class=hs-linenum>174: </span>    <a class=annot href="#"><span class=annottext>{v : Int | notEmpty xs =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>elems</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>175: </span>    <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-varid'>ok</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>    <span class='hs-comment'>-- What expression goes here? </span>
</pre>


\exercise An important aspect of formal verifiers like LiquidHaskell
is that they help establish properties not just of your *implementations*
but equally, or more importantly, of your *specifications*. In that spirit,
can you explain why the following two variants of `size` are *rejected*
by LiquidHaskell? 


<pre><span class=hs-linenum>186: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>size1</span>    <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-layout'>(</span><span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Pos</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>187: </span><a class=annot href="#"><span class=annottext>forall a. {v : [a] | notEmpty v} -&gt; {v : Int | 0 &lt; v}</span><span class='hs-definition'>size1</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>188: </span><span class='hs-definition'>size1</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>forall a. {v : [a] | notEmpty v} -&gt; {v : Int | 0 &lt; v}</span><span class='hs-varid'>size1</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>
<span class=hs-linenum>189: </span>
<span class=hs-linenum>190: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>size2</span>    <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Int</span> <span class='hs-keyword'>| notEmpty xs =&gt; v &gt; 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>191: </span><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0}</span><span class='hs-definition'>size2</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>192: </span><span class='hs-definition'>size2</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span>  <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : Int | notEmpty x1 =&gt; v &gt; 0}</span><span class='hs-varid'>size2</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>
</pre>

\todo {} **solution**

A Safe List API 
---------------

Now that we can talk about non-empty lists, we can ensure
the safety of various list-manipulating functions which
are only well-defined on non-empty lists and which crash
with unexpected run-time errors otherwise.

\newthought{Heads and Tails}
For example, we can type the potentially dangerous `head`
and `tail` as:


<pre><span class=hs-linenum>210: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>head</span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>211: </span><a class=annot href="#"><span class=annottext>forall a. {v : [a] | notEmpty v} -&gt; a</span><span class='hs-definition'>head</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>
<span class=hs-linenum>212: </span><span class='hs-definition'>head</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Fear not! 'twill ne'er come to pass"</span></a>
<span class=hs-linenum>213: </span>
<span class=hs-linenum>214: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>tail</span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>215: </span><a class=annot href="#"><span class=annottext>forall a. {v : [a] | notEmpty v} -&gt; [a]</span><span class='hs-definition'>tail</span></a> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>216: </span><span class='hs-definition'>tail</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; [a]</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Relaxeth! this too shall ne'er be"</span></a>
</pre>

LiquidHaskell deduces that the second equations are
*dead code* thanks to the precondition, which ensures
callers only supply non-empty arguments.

\exercise Write down a specification for `null` such that `safeHead` is verified:


<pre><span class=hs-linenum>226: </span><span class='hs-definition'>safeHead</span>      <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>227: </span><a class=annot href="#"><span class=annottext>forall a. [a] -&gt; (Maybe a)</span><span class='hs-definition'>safeHead</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>228: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>[a] -&gt; Bool</span><span class='hs-varid'>null</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Maybe a) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a>
<span class=hs-linenum>229: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a -&gt; {v : (Maybe a) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <a class=annot href="#"><span class=annottext>(a -&gt; (Maybe a)) -&gt; a -&gt; (Maybe a)</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v} -&gt; a</span><span class='hs-varid'>head</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>  
<span class=hs-linenum>230: </span>
<span class=hs-linenum>231: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>null</span>      <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>232: </span><a class=annot href="#"><span class=annottext>forall a. [a] -&gt; Bool</span><span class='hs-definition'>null</span></a> <span class='hs-conid'>[]</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a> 
<span class=hs-linenum>233: </span><span class='hs-definition'>null</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
</pre>

\newthought{Groups}
Lets use the above to write a function that chunks sequences
into non-empty groups of equal elements:


<pre><span class=hs-linenum>241: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>groupEq</span>         <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Eq</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>242: </span><a class=annot href="#"><span class=annottext>forall a. (Eq a) =&gt; [a] -&gt; [{v : [a] | notEmpty v}]</span><span class='hs-definition'>groupEq</span></a> <span class='hs-conid'>[]</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>243: </span><span class='hs-definition'>groupEq</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>      <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len xs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:{v : [a] | notEmpty v}
-&gt; x2:[{v : [a] | notEmpty v}]&lt;\_ VV -&gt; notEmpty v&gt;
-&gt; {v : [{v : [a] | notEmpty v}]&lt;\_ VV -&gt; notEmpty v&gt; | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a. (Eq a) =&gt; [a] -&gt; [{v : [a] | notEmpty v}]</span><span class='hs-varid'>groupEq</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len xs}</span><span class='hs-varid'>zs</span></a>
<span class=hs-linenum>244: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>245: </span>    <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len xs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : [a] | VV == zs &amp;&amp; len VV == len zs &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len xs}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(a -&gt; Bool)
-&gt; x3:[a]
-&gt; ({v : [a] | len v &lt;= len x3}, {v : [a] | len v &lt;= len x3})</span><span class='hs-varid'>span</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\noindent By using the fact that *each element* in the
output returned by `groupEq` is in fact of the form `x:ys`,
LiquidHaskell infers that `groupEq` returns a `[NEList a]`
that is, a list of *non-empty lists*.

We can use `groupEq` to write a function that
eliminates stuttering from a String:


<pre><span class=hs-linenum>257: </span><span class='hs-comment'>-- &gt;&gt;&gt; eliminateStutter "ssstringssss liiiiiike thisss"</span>
<span class=hs-linenum>258: </span><span class='hs-comment'>-- "strings like this"</span>
<span class=hs-linenum>259: </span><a class=annot href="#"><span class=annottext>forall a. (Eq a) =&gt; [a] -&gt; [a]</span><span class='hs-definition'>eliminateStutter</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; [a] -&gt; [b]</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v} -&gt; a</span><span class='hs-varid'>head</span></a> <a class=annot href="#"><span class=annottext>({v : [{v : [a] | notEmpty v}] | len v &gt;= 0} -&gt; [a])
-&gt; {v : [{v : [a] | notEmpty v}] | len v &gt;= 0} -&gt; [a]</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>forall a. (Eq a) =&gt; [a] -&gt; [{v : [a] | notEmpty v}]</span><span class='hs-varid'>groupEq</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\noindent
LiquidHaskell automatically instantiates the type parameter
for `map` in `eliminateStutter` to `notEmpty v` to deduce that
`head` is only called on non-empty lists.

\newthought{Folds} One of my favorite folds is `foldr1` which
uses the first element of the sequence as the initial value. Of course,
it should only be called with non-empty sequences!


<pre><span class=hs-linenum>272: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>foldr1</span>      <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>273: </span><a class=annot href="#"><span class=annottext>forall a. (a -&gt; a -&gt; a) -&gt; {v : [a] | notEmpty v} -&gt; a</span><span class='hs-definition'>foldr1</span></a> <a class=annot href="#"><span class=annottext>a -&gt; a -&gt; a</span><span class='hs-varid'>f</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b -&gt; b) -&gt; b -&gt; [a] -&gt; b</span><span class='hs-varid'>foldr</span></a> <a class=annot href="#"><span class=annottext>a -&gt; a -&gt; a</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>274: </span><span class='hs-definition'>foldr1</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"foldr1"</span></a> 
<span class=hs-linenum>275: </span>
<span class=hs-linenum>276: </span><span class='hs-definition'>foldr</span>              <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> 
<span class=hs-linenum>277: </span><a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b -&gt; b) -&gt; b -&gt; [a] -&gt; b</span><span class='hs-definition'>foldr</span></a> <span class='hs-keyword'>_</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>acc</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == acc}</span><span class='hs-varid'>acc</span></a>
<span class=hs-linenum>278: </span><span class='hs-definition'>foldr</span> <span class='hs-varid'>f</span> <span class='hs-varid'>acc</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b -&gt; b) -&gt; b -&gt; [a] -&gt; b</span><span class='hs-varid'>foldr</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == acc}</span><span class='hs-varid'>acc</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{Sum}
Thanks to the precondition, LiquidHaskell will prove that
the `die` code is indeed dead. Thus, we can write


<pre><span class=hs-linenum>286: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>sum</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Num</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>287: </span><a class=annot href="#"><span class=annottext>forall a. (Num a) =&gt; {v : [a] | notEmpty v} -&gt; a</span><span class='hs-definition'>sum</span></a> <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"cannot add up empty list"</span></a>
<span class=hs-linenum>288: </span><span class='hs-definition'>sum</span> <span class='hs-varid'>xs</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(a -&gt; a -&gt; a) -&gt; {v : [a] | notEmpty v} -&gt; a</span><span class='hs-varid'>foldr1</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {VV : a | VV == x1 + x2}</span><span class='hs-layout'>(</span></a><span class='hs-varop'>+</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\noindent Consequently, we can only invoke `sum` on non-empty lists, so:


<pre><span class=hs-linenum>294: </span><a class=annot href="#"><span class=annottext>Integer</span><span class='hs-definition'>sumOk</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v} -&gt; Integer</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><span class='hs-num'>1</span><span class='hs-layout'>,</span><span class='hs-num'>2</span><span class='hs-layout'>,</span><span class='hs-num'>3</span><span class='hs-layout'>,</span><span class='hs-num'>4</span><span class='hs-layout'>,</span><span class='hs-num'>5</span><span class='hs-keyglyph'>]</span>    <span class='hs-comment'>-- accepted by LH</span>
<span class=hs-linenum>295: </span>
<span class=hs-linenum>296: </span><a class=annot href="#"><span class=annottext>Integer</span><span class='hs-definition'>sumBad</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v} -&gt; Integer</span><span class='hs-varid'>sum</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Integer] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a></span>             <span class='hs-comment'>-- rejected by LH</span>
</pre>

\exercise The function below computes a weighted average of its input.
Unfortunately, LiquidHaskell is not very happy about it. Can you figure out
why, and fix the code or specification appropriately?


<pre><span class=hs-linenum>304: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>wtAverage</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>NEList</span> <span class='hs-layout'>(</span><span class='hs-conid'>Pos</span><span class='hs-layout'>,</span> <span class='hs-conid'>Pos</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>305: </span><a class=annot href="#"><span class=annottext>{v : [({v : Int | 0 &lt; v}, {v : Int | 0 &lt; v})] | notEmpty v} -&gt; Int</span><span class='hs-definition'>wtAverage</span></a> <a class=annot href="#"><span class=annottext>{v : [({v : Int | 0 &lt; v}, {v : Int | 0 &lt; v})] | notEmpty v}</span><span class='hs-varid'>wxs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | 0 /= v} -&gt; Int</span><span class='hs-varid'>divide</span></a> <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v == totElems &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>totElems</span></a> <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v == totWeight &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>totWeight</span></a> 
<span class=hs-linenum>306: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>307: </span>    <a class=annot href="#"><span class=annottext>[{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}]</span><span class='hs-varid'>elems</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; [a] -&gt; [b]</span><span class='hs-varid'>map</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>({v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}, {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v})
-&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-keyglyph'>\</span></a><span class='hs-layout'>(</span><span class='hs-varid'>w</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v == w &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>w</span></a> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:Int
-&gt; {v : Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a> <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v == x &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [({v : Int | 0 &lt; v}, {v : Int | 0 &lt; v})] | notEmpty v &amp;&amp; v == wxs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>wxs</span></a>
<span class=hs-linenum>308: </span>    <a class=annot href="#"><span class=annottext>[{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}]</span><span class='hs-varid'>weights</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; [a] -&gt; [b]</span><span class='hs-varid'>map</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:({v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}, {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v})
-&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v == fst x1 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-keyglyph'>\</span></a><span class='hs-layout'>(</span><span class='hs-varid'>w</span><span class='hs-layout'>,</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v == w &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>w</span></a>    <span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [({v : Int | 0 &lt; v}, {v : Int | 0 &lt; v})] | notEmpty v &amp;&amp; v == wxs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>wxs</span></a>
<span class=hs-linenum>309: </span>    <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>totElems</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}] | notEmpty v}
-&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>sum</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; notEmpty weights =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}] | v == elems &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>elems</span></a></span> 
<span class=hs-linenum>310: </span>    <a class=annot href="#"><span class=annottext>{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>totWeight</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}] | notEmpty v}
-&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>sum</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}] | v == weights &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>weights</span></a></span> 
<span class=hs-linenum>311: </span>    <a class=annot href="#"><span class=annottext>{v : [{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}] | notEmpty v}
-&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>sum</span></a>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>({v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}
 -&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}
 -&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v})
-&gt; {v : [{v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}] | notEmpty v}
-&gt; {v : Int | notEmpty wxs =&gt; v &gt; 0 &amp;&amp; v &gt; 0 &amp;&amp; 0 &lt; v}</span><span class='hs-varid'>foldr1</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + v}</span><span class='hs-layout'>(</span></a><span class='hs-varop'>+</span><span class='hs-layout'>)</span>
<span class=hs-linenum>312: </span>
<span class=hs-linenum>313: </span><span class='hs-definition'>map</span>           <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span> 
<span class=hs-linenum>314: </span><a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; [a] -&gt; [b]</span><span class='hs-definition'>map</span></a> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>315: </span><span class='hs-definition'>map</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a b. (a -&gt; b) -&gt; [a] -&gt; [b]</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> 
</pre>

\hint On what variables are the errors? How are those variables' values computed?
Can you think of a better specification for the function(s) doing those computations?


\exercise
Non-empty lists pop up in many places, and it is rather convenient
to have the type system track non-emptiness without having to make
up special types. Consider the `risers` function:
\footnotetext{Popularized by [Neil Mitchell](http://neilmitchell.blogspot.com/2008/03/sorting-at-speed.html)}


<pre><span class=hs-linenum>329: </span><span class='hs-definition'>risers</span>           <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>330: </span><a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; [[a]]</span><span class='hs-definition'>risers</span></a> <span class='hs-conid'>[]</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>331: </span><span class='hs-definition'>risers</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>x</span><span class='hs-keyglyph'>]</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [[a]] | notEmpty v &lt;=&gt; false &amp;&amp; null v &lt;=&gt; true &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>332: </span><span class='hs-definition'>risers</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>etc</span><span class='hs-layout'>)</span>
<span class=hs-linenum>333: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>       <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:[a]
-&gt; x2:[[a]]
-&gt; {v : [[a]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == ss &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ss</span></a>
<span class=hs-linenum>334: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-keyglyph'>]</span> <a class=annot href="#"><span class=annottext>x1:[a]
-&gt; x2:[[a]]
-&gt; {v : [[a]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : [a] | v == s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>x1:[a]
-&gt; x2:[[a]]
-&gt; {v : [[a]] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == ss &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ss</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>335: </span>    <span class='hs-keyword'>where</span> 
<span class=hs-linenum>336: </span>      <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>s</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>[[a]]</span><span class='hs-varid'>ss</span></a><span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [[a]] | notEmpty v} -&gt; ([a], [[a]])</span><span class='hs-varid'>safeSplit</span></a></span> <a class=annot href="#"><span class=annottext>({v : [[a]] | len v &gt;= 0} -&gt; ([a], [[a]]))
-&gt; {v : [[a]] | len v &gt;= 0} -&gt; ([a], [[a]])</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>forall a. (Ord a) =&gt; [a] -&gt; [[a]]</span><span class='hs-varid'>risers</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | notEmpty v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == etc &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>etc</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>337: </span>
<span class=hs-linenum>338: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>safeSplit</span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>NEList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>339: </span><a class=annot href="#"><span class=annottext>forall a. {v : [a] | notEmpty v} -&gt; (a, [a])</span><span class='hs-definition'>safeSplit</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x6 VV -&gt; p2 x6&gt; | fst v == x1 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; snd v == x2 &amp;&amp; x_Tuple21 v == x1}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>340: </span><span class='hs-definition'>safeSplit</span> <span class='hs-keyword'>_</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; (a, [a])</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"don't worry, be happy"</span></a>
</pre>

\noindent The call to `safeSplit` requires its input be non-empty,
and LiquidHaskell does not believe that the call inside `risers`
meets this requirement. Can you devise a specification for `risers`
that allows LiquidHaskell to verify the call to `safeSplit` that
`risers` will not `die`?

Recap
-----

In this chapter we saw how LiquidHaskell lets you 

1. Define *structural properties* of data types, 

2. Use refinements over these properties to describe key
   invariants that establish, at compile-time, the safety
   of operations that might otherwise fail on unexpected
   values at run-time, all while,

3. Working with plain Haskell types, here, Lists, without
   having to [make up new types][apple-riser]
   which can have the unfortunate effect of adding a multitude of constructors
   and conversions which often clutter implementations and specifications.

\noindent 
Of course, We can do a lot more with measures, so lets press on!



