

Elemental Measures {#setmeasure}
================


\begin{comment}

<pre><span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>11: </span>
<span class=hs-linenum>12: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>Sets</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>13: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Set</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>insert</span><span class='hs-layout'>,</span> <span class='hs-varid'>partition</span><span class='hs-layout'>,</span> <span class='hs-varid'>filter</span><span class='hs-layout'>,</span> <span class='hs-varid'>split</span><span class='hs-layout'>,</span> <span class='hs-varid'>elems</span><span class='hs-layout'>)</span>
<span class=hs-linenum>14: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span>  <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>elem</span><span class='hs-layout'>,</span> <span class='hs-varid'>reverse</span><span class='hs-layout'>,</span> <span class='hs-varid'>filter</span><span class='hs-layout'>)</span>
<span class=hs-linenum>15: </span>
<span class=hs-linenum>16: </span><span class='hs-definition'>main</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>17: </span><a class=annot href="#"><span class=annottext>(GHC.Types.IO ())</span><span class='hs-definition'>main</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>() -&gt; (GHC.Types.IO ())</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
<span class=hs-linenum>18: </span>
<span class=hs-linenum>19: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>die</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| false}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>20: </span><a class=annot href="#"><span class=annottext>forall a. {v : [GHC.Types.Char] | false} -&gt; a</span><span class='hs-definition'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false}</span><span class='hs-varid'>msg</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[GHC.Types.Char] -&gt; a</span><span class='hs-varid'>error</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false}</span><span class='hs-varid'>msg</span></a>
<span class=hs-linenum>21: </span>
<span class=hs-linenum>22: </span><span class='hs-definition'>isUnique</span><span class='hs-layout'>,</span> <span class='hs-varid'>isNotUnique</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>23: </span><span class='hs-definition'>mergeSort</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>24: </span><span class='hs-definition'>range</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>25: </span>
<span class=hs-linenum>26: </span><span class='hs-comment'>-- TODO: qualifier needed for focus* ? not clear. Eric: can you check?</span>
<span class=hs-linenum>27: </span><span class='hs-comment'>{- q0 :: x:a -&gt;  {v:[a] | not (Elem x v)} @-}</span>
<span class=hs-linenum>28: </span><span class='hs-definition'>q0</span>   <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>29: </span><a class=annot href="#"><span class=annottext>forall a. a -&gt; [a]</span><span class='hs-definition'>q0</span></a> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x7 VV -&gt; p x7&gt; | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
</pre>
\end{comment}

Often, correctness requires us to reason about the *set of elements*
represented inside a data structure, or manipulated by a function.

\newthought{Sets} appear everywhere. For example, we'd like to know that:

+ *sorting* routines return permutations of their inputs --
  i.e. return collections whose elements are the same as the
  input' set,

+ *resource management* functions do not inadvertently
  create duplicate elements or drop  elements from set
  of tracked resources.

+ *syntax-tree* manipulating procedures create well-scoped
  trees where (the set of) used variables are (contained
  within the set of variables) previously defined.

\newthought{SMT Solvers} support rather expressive logics. In addition to the
linear arithmetic and uninterpreted functions, they can [efficiently decide](smt-set)
formulas over sets. Next, lets see how LiquidHaskell lets us exploit this fact
to develop types and interfaces that guarantee invariants over the (set of)
elements of a structures.

Talking about Sets
------------------

First, we need a way to talk about sets in the refinement logic. We could
roll our own special Haskell type
\footnotetext{See [this](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2013/03/26/talking-about-sets.lhs/) for a brief description of how to do so},
but for now, lets just use the `Set a` type from the prelude's `Data.Set`.

\newthought{Lifted Operators} The LiquidHaskell prelude *lifts* the basic set
operators from `Data.Set` into the refinement logic, i.e. defines the following
logical functions that correspond to the Haskell functions of the same name:


<pre><span class=hs-linenum>69: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>empty</span>        <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>70: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>singleton</span>    <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>71: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>member</span>       <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>  
<span class=hs-linenum>72: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>union</span>        <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>73: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>intersection</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>74: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>difference</span>   <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
</pre>

\newthought{Interpreted Operators}
The above operators are *interpreted* by the SMT solver.
That is, just like the SMT solver "knows", via the
axioms of the theory of arithmetic that:
$$x = 2 + 2 \Rightarrow x = 4$$
is a valid formula, i.e. holds for all $x$, the solver "knows" that:
$$x = \tsng{1} \Rightarrow y = \tsng{2} \Rightarrow x = \tcap{x}{\tcup{y}{x}}$$
This is because, the above formulas belong to a decidable Theory of Sets
reduces to McCarthy's more general [Theory of Arrays][mccarthy]. 
\footnotetext{See [this recent paper](http://research.microsoft.com/en-us/um/people/leonardo/fmcad09.pdf) to learn how modern SMT solvers prove equalities like the above.}


Proving QuickCheck Style Properties {#quickcheck} 
-----------------------------------

To get the hang of whats going on, lets do a few warmup exercises,
using LiquidHaskell to prove various simple "theorems" about sets
and operations over them.

\newthought{Refined Set API} To make it easy to write down theorems,
we've refined the types of the operators in `Data.Set` so that they
mirror their logical counterparts:


<pre><span class=hs-linenum>101: </span><span class='hs-definition'>empty</span>        <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>empty</span><span class='hs-layout'>}</span> 
<span class=hs-linenum>102: </span><span class='hs-definition'>singleton</span>    <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>singleton</span> <span class='hs-varid'>x</span><span class='hs-layout'>}</span> 
<span class=hs-linenum>103: </span><span class='hs-definition'>union</span>        <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>union</span> <span class='hs-varid'>x</span> <span class='hs-varid'>y</span><span class='hs-layout'>}</span>
<span class=hs-linenum>104: </span><span class='hs-definition'>intersection</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>intersection</span> <span class='hs-varid'>x</span> <span class='hs-varid'>y</span><span class='hs-layout'>}</span>
<span class=hs-linenum>105: </span><span class='hs-definition'>difference</span>   <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>difference</span> <span class='hs-varid'>x</span> <span class='hs-varid'>y</span><span class='hs-layout'>}</span>
<span class=hs-linenum>106: </span><span class='hs-definition'>member</span>       <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>s</span><span class='hs-conop'>:</span><span class='hs-conid'>Set</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Bool</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Prop</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;=&gt;</span> <span class='hs-varid'>member</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span><span class='hs-layout'>}</span>  
</pre>

\newthought{Asserting Properties} Lets write our theorems
as [QuickCheck](quickcheck) style *properties*, that is,
as functions from arbitrary inputs to a `Bool` output
that must always be `True`. Lets define aliases for
the singletons `True` and `False`:


<pre><span class=hs-linenum>116: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>True</span>  <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Bool</span> <span class='hs-keyglyph'>|</span>      <span class='hs-conid'>Prop</span> <span class='hs-varid'>v</span> <span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>117: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>False</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Bool</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>not</span> <span class='hs-layout'>(</span><span class='hs-conid'>Prop</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent We can use `True` to state and prove theorems. For example,
something (boring) like the arithmetic equality above becomes:


<pre><span class=hs-linenum>124: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_one_plus_one_eq_two</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>125: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Num.Num a, GHC.Classes.Eq a) =&gt;
a -&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_one_plus_one_eq_two</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a>   <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {VV : a | VV == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; x2:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Prop x1 =&gt; Prop v}</span><span class='hs-varop'>`implies`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span>
</pre>

\noindent Where `implies` is just the implication function over ``Bool``


<pre><span class=hs-linenum>131: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>implies</span>        <span class='hs-keyglyph'>::</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>q</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v &lt;=&gt; (Prop p =&gt; Prop q)}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>132: </span><a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; x2:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Prop x1 =&gt; Prop x2}</span><span class='hs-definition'>implies</span></a> <span class='hs-conid'>False</span> <span class='hs-keyword'>_</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>
<span class=hs-linenum>133: </span><span class='hs-definition'>implies</span> <span class='hs-keyword'>_</span>     <span class='hs-conid'>True</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a> 
<span class=hs-linenum>134: </span><span class='hs-definition'>implies</span> <span class='hs-keyword'>_</span>    <span class='hs-keyword'>_</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
<span class=hs-linenum>135: </span><span class='hs-comment'>-- implies p q = not p || q</span>
</pre>

\exercisen{Bounded Addition} Write a QuickCheck style proof of the fact
that $x < 100 \wedge y < 100 \Rightarrow x + y < 200$.


<pre><span class=hs-linenum>142: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_x_y_200</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>143: </span><a class=annot href="#"><span class=annottext>forall a b. a -&gt; b -&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_x_y_200</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a></span> <span class='hs-comment'>-- fill in the appropriate body to obtain the theorem. </span>
</pre>


\newthought{Intersection is Commutative} Ok, lets prove things about
sets and their operators! First, lets check that  `intersection` is
commutative:


<pre><span class=hs-linenum>152: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_intersection_comm</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>153: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
(Data.Set.Base.Set a)
-&gt; (Data.Set.Base.Set a) -&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_intersection_comm</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>y</span></a> 
<span class=hs-linenum>154: </span>  <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cap x1 v}</span><span class='hs-varop'>`intersection`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cap x1 v}</span><span class='hs-varop'>`intersection`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{Union is Associative} Similarly, we might verify
that union is associative:


<pre><span class=hs-linenum>161: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_intersection_comm</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>162: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
(Data.Set.Base.Set a)
-&gt; (Data.Set.Base.Set a) -&gt; (Data.Set.Base.Set a) -&gt; GHC.Types.Bool</span><span class='hs-definition'>prop_union_assoc</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>z</span></a> 
<span class=hs-linenum>163: </span>  <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == z}</span><span class='hs-varid'>z</span></a><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == z}</span><span class='hs-varid'>z</span></a>
</pre>

\newthought{Union Distributes over Intersection} and while we're at it,
check various distributivity laws of Boolean algebra:


<pre><span class=hs-linenum>170: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_intersection_dist</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>171: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
(Data.Set.Base.Set a)
-&gt; (Data.Set.Base.Set a)
-&gt; (Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_intersection_dist</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>z</span></a> 
<span class=hs-linenum>172: </span>  <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cap x1 v}</span><span class='hs-varop'>`intersection`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == z}</span><span class='hs-varid'>z</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cap x1 v}</span><span class='hs-varop'>`intersection`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cap x1 v}</span><span class='hs-varop'>`intersection`</span></a> <a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == z}</span><span class='hs-varid'>z</span></a><span class='hs-layout'>)</span> 
</pre>

\newthought{Non-Theorems}
Of course, while we're at it, let's make sure LiquidHaskell
doesn't prove anything that *isn't* true ...


<pre><span class=hs-linenum>180: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_cup_dif_bad</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>181: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
(Data.Set.Base.Set a)
-&gt; (Data.Set.Base.Set a) -&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_cup_dif_bad</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-varid'>y</span></a>
<span class=hs-linenum>182: </span>  <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True &amp;&amp; v == pre}</span><span class='hs-varid'>pre</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; x2:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Prop x1 =&gt; Prop v}</span><span class='hs-varop'>`implies`</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-layout'>(</span></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-layout'>(</span></span><span class=hs-error><a class=annot href="#"><span class=annottext>(Data.Set.Base.Set a)</span><span class='hs-layout'>(</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == x}</span><span class='hs-varid'>x</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_dif x1 v}</span><span class='hs-varop'>`difference`</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Data.Set.Base.Set a) | v == y}</span><span class='hs-varid'>y</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error><span class='hs-layout'>)</span></span>
<span class=hs-linenum>183: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>184: </span>    <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-varid'>pre</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>  <span class='hs-comment'>-- Fix this with a non-trivial precondition</span>
</pre>

\exercisen{Set Difference} Do you know why the above fails?
1. Use QuickCheck to find a *counterexample* for the property `prop_cup_dif_bad`, and,
2. Use the counterexample to assign `pre` a non-trivial (i.e. non `False`) condition
   so that the property can be proved.

Thus, LiquidHaskell's refined types offer a nice interface
for interacting with the SMT solvers in order to *prove*
theorems, while letting us use QuickCheck to generate
counterexamples.
\footnotetext{The [SBV](https://github.com/LeventErkok/sbv)
and [Leon](http://lara.epfl.ch/w/leon) projects describe
a different DSL based approach for using SMT solvers
from Haskell and Scala respectively.}

Content-Aware List API {#listelems}
----------------------------------

Our overall goal is to verify properties of programs.
Lets start off by refining the list API to precisely
track the list elements.

\newthought{Elements of a List} To specify the permutation
property, we need a way to talk about the set of elements
in a list. At this point, hopefully you know what we're
going to do: write a measure!


<pre><span class=hs-linenum>214: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>elems</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>215: </span><span class='hs-definition'>elems</span>        <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>216: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-definition'>elems</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Data.Set.Base.Set a) | Set_emp v}</span><span class='hs-varid'>empty</span></a>
<span class=hs-linenum>217: </span><span class='hs-definition'>elems</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a -&gt; {v : (Data.Set.Base.Set a) | v == Set_sng x1}</span><span class='hs-varid'>singleton</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\newthought{Strengthened Constructors}
Recall, that as before, the above definition automatically
strengthens the types for the constructors:


<pre><span class=hs-linenum>225: </span><span class='hs-keyword'>data</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>226: </span>  <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>empty</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>227: </span>  <span class='hs-layout'>(</span><span class='hs-conop'>:</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>elems</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>union</span> <span class='hs-layout'>(</span><span class='hs-varid'>singleton</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span><span class='hs-layout'>}</span>
</pre>

Next, to make the specifications concise, let's define a few predicate aliases:


<pre><span class=hs-linenum>233: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>EqElts</span>  <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span>   <span class='hs-keyglyph'>=</span> <span class='hs-varid'>elems</span> <span class='hs-conid'>X</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>elems</span> <span class='hs-conid'>Y</span>                         <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>234: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>SubElts</span> <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_sub</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>X</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Y</span><span class='hs-layout'>)</span>               <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>235: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>DisjElts</span> <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span>  <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_empty</span> <span class='hs-num'>0</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_cap</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>X</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Y</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>236: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>Empty</span>   <span class='hs-conid'>X</span>     <span class='hs-keyglyph'>=</span> <span class='hs-varid'>elems</span> <span class='hs-conid'>X</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_empty</span> <span class='hs-num'>0</span>                     <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>237: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>UnElts</span>  <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span> <span class='hs-conid'>Z</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>elems</span> <span class='hs-conid'>X</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_cup</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Y</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Z</span><span class='hs-layout'>)</span>     <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>238: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>UnElt</span>   <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span> <span class='hs-conid'>Z</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>elems</span> <span class='hs-conid'>X</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_cup</span> <span class='hs-layout'>(</span><span class='hs-conid'>Set_sng</span> <span class='hs-conid'>Y</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Z</span><span class='hs-layout'>)</span>   <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>239: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>Elem</span>    <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_mem</span> <span class='hs-conid'>X</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Y</span><span class='hs-layout'>)</span>                       <span class='hs-keyword'>@-}</span>
</pre>


\newthought{Append}
First, here's good old `append`, but now with a specification that states
that the output indeed includes the elements from both the input lists.


<pre><span class=hs-linenum>248: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>append'</span>       <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>ys</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| UnElts v xs ys}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>249: </span><a class=annot href="#"><span class=annottext>forall a.
x1:[a]
-&gt; x2:[a] -&gt; {v : [a] | elems v == Set_cup elems x1 elems x2}</span><span class='hs-definition'>append'</span></a> <span class='hs-conid'>[]</span>     <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>ys</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>250: </span><span class='hs-definition'>append'</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varid'>ys</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a.
x1:[a]
-&gt; x2:[a] -&gt; {v : [a] | elems v == Set_cup elems x1 elems x2}</span><span class='hs-varid'>append'</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
</pre>

\exercisen{Reverse} Write down a type for `revHelper` so that `reverse'`
is verified by LiquidHaskell:


<pre><span class=hs-linenum>257: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>reverse'</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| EqElts v xs}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>258: </span><a class=annot href="#"><span class=annottext>forall a. x1:[a] -&gt; {v : [a] | elems v == elems x1}</span><span class='hs-definition'>reverse'</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>forall a. [a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>revHelper</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>
<span class=hs-linenum>259: </span>
<span class=hs-linenum>260: </span><a class=annot href="#"><span class=annottext>forall a. [a] -&gt; [a] -&gt; [a]</span><span class='hs-definition'>revHelper</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>acc</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == acc &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>acc</span></a>
<span class=hs-linenum>261: </span><span class='hs-definition'>revHelper</span> <span class='hs-varid'>acc</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>revHelper</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == acc &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>acc</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\exercisen{Partition} \singlestar Write down a
specification for `split` such that the subsequent
"theorem" `prop_partition_appent` is proved by LiquidHaskell.


<pre><span class=hs-linenum>269: </span><span class='hs-definition'>split</span>            <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span>
<span class=hs-linenum>270: </span><a class=annot href="#"><span class=annottext>forall a. GHC.Types.Int -&gt; [a] -&gt; ([a], [a])</span><span class='hs-definition'>split</span></a> <span class='hs-num'>0</span> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x6 VV -&gt; p2 x6&gt; | fst v == x1 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; snd v == x2 &amp;&amp; x_Tuple21 v == x1}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>271: </span><span class='hs-definition'>split</span> <span class='hs-varid'>n</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>zs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x6 VV -&gt; p2 x6&gt; | fst v == x1 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; snd v == x2 &amp;&amp; x_Tuple21 v == x1}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <span class='hs-keyword'>where</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. GHC.Types.Int -&gt; [a] -&gt; ([a], [a])</span><span class='hs-varid'>split</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs</span></a>
<span class=hs-linenum>272: </span><span class='hs-definition'>split</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>xs</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b &lt;p2 :: a b -&gt; Prop&gt;.
x1:a
-&gt; x2:{VV : b&lt;p2 x1&gt; | true}
-&gt; {v : (a, b)&lt;\x6 VV -&gt; p2 x6&gt; | fst v == x1 &amp;&amp; x_Tuple22 v == x2 &amp;&amp; snd v == x2 &amp;&amp; x_Tuple21 v == x1}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>273: </span>
<span class=hs-linenum>274: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_split_append</span>  <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>275: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
GHC.Types.Int -&gt; [a] -&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_split_append</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs' &amp;&amp; elems v == Set_cup elems ys elems zs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs'</span></a></span>
<span class=hs-linenum>276: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>277: </span>    <a class=annot href="#"><span class=annottext>{v : [a] | elems v == Set_cup elems ys elems zs}</span><span class='hs-varid'>xs'</span></a>      <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>forall a.
x1:[a]
-&gt; x2:[a] -&gt; {v : [a] | elems v == Set_cup elems x1 elems x2}</span><span class='hs-varid'>append'</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; elems v == Set_cup elems ys elems ys &amp;&amp; elems v == elems ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; elems v == Set_cup elems zs elems zs &amp;&amp; elems v == elems zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs</span></a>
<span class=hs-linenum>278: </span>    <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; elems VV == Set_cup elems ys elems ys &amp;&amp; elems VV == elems ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : [a] | VV == zs &amp;&amp; elems VV == Set_cup elems zs elems zs &amp;&amp; elems VV == elems zs &amp;&amp; len VV == len zs &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>forall a. GHC.Types.Int -&gt; [a] -&gt; ([a], [a])</span><span class='hs-varid'>split</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == n}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> 
</pre>

\hint You may want to remind yourself about the
"dimension-aware" signature for `partition` from
[the earlier chapter](#listreducing).

\exercisen{Membership} Write down a signature for `elem`
that suffices to verify `test1` and `test2` by LiquidHaskell.


<pre><span class=hs-linenum>289: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>elem</span>      <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Eq</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>290: </span><a class=annot href="#"><span class=annottext>forall a. (GHC.Classes.Eq a) =&gt; a -&gt; [a] -&gt; GHC.Types.Bool</span><span class='hs-definition'>elem</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>ys</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; x2:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Prop x1 || Prop v}</span><span class='hs-varop'>||</span></a> <a class=annot href="#"><span class=annottext>forall a. (GHC.Classes.Eq a) =&gt; a -&gt; [a] -&gt; GHC.Types.Bool</span><span class='hs-varid'>elem</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>291: </span><span class='hs-definition'>elem</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
<span class=hs-linenum>292: </span>
<span class=hs-linenum>293: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test1</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>True</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>294: </span><a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>test1</span></a>      <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>forall a. (GHC.Classes.Eq a) =&gt; a -&gt; [a] -&gt; GHC.Types.Bool</span><span class='hs-varid'>elem</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>2</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><span class='hs-num'>1</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error><span class='hs-num'>2</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error><span class='hs-num'>3</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span>
<span class=hs-linenum>295: </span>
<span class=hs-linenum>296: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>test2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>False</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>297: </span><a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v)}</span><span class='hs-definition'>test2</span></a>      <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>forall a. (GHC.Classes.Eq a) =&gt; a -&gt; [a] -&gt; GHC.Types.Bool</span><span class='hs-varid'>elem</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-num'>2</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><span class='hs-num'>1</span></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error><span class='hs-num'>3</span></span><span class=hs-error><span class='hs-keyglyph'>]</span></span> 
</pre>

Permutations
------------

Next, lets use the refined list API to prove that
various list-sorting routines return *permutations*
of their inputs, that is, return output lists whose
elements are the *same as* those of the input lists.
Since we are focusing on the elements, lets not
distract ourselves with the ordering invariant
just, and reuse plain old lists.
\footnotetext{See [this](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2013/07/29/putting-things-in-order.lhs/) for how to
specify and verify order with plain old lists.}

\newthought{InsertionSort} is the simplest of all the
list sorting routines; we build up an (ordered) output
list `insert`ing each element of the input list into
the appropriate position of the output:


<pre><span class=hs-linenum>319: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup Set_sng x2 elems x3}</span><span class='hs-definition'>insert</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>320: </span><span class='hs-definition'>insert</span> <span class='hs-varid'>x</span> <span class='hs-layout'>(</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>ys</span><span class='hs-layout'>)</span>
<span class=hs-linenum>321: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>322: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup Set_sng x2 elems x3}</span><span class='hs-varid'>insert</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
</pre>

\noindent Thus, the output of `insert` has all the
elements of the input `xs`, plus the new element `x`:


<pre><span class=hs-linenum>329: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>insert</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| UnElt v x xs }</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent Which then lets us prove that the output
of the sorting routine indeed has the elements of the input:


<pre><span class=hs-linenum>336: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>insertSort</span>    <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| EqElts v xs}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>337: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {v : [a] | elems v == elems x2}</span><span class='hs-definition'>insertSort</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x7 VV -&gt; p x7&gt; | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>338: </span><span class='hs-definition'>insertSort</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup Set_sng x2 elems x3}</span><span class='hs-varid'>insert</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {v : [a] | elems v == elems x2}</span><span class='hs-varid'>insertSort</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
</pre>

\exercisen{Merge}
Write down a specification of `merge` such that the subsequent property is
verified by LiquidHaskell:


<pre><span class=hs-linenum>346: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>merge</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>ys</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| UnElts v xs ys}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>347: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a]
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup elems x2 elems x3}</span><span class='hs-definition'>merge</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>ys</span><span class='hs-layout'>)</span>
<span class=hs-linenum>348: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a]
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup elems x2 elems x3}</span><span class='hs-varid'>merge</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>349: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a]
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup elems x2 elems x3}</span><span class='hs-varid'>merge</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>350: </span><span class='hs-definition'>merge</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>ys</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>351: </span><span class='hs-definition'>merge</span> <span class='hs-varid'>xs</span> <span class='hs-conid'>[]</span>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>352: </span>
<span class=hs-linenum>353: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_merge_app</span>   <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>True</span>   <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>354: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
[a] -&gt; [a] -&gt; {v : GHC.Types.Bool | Prop v}</span><span class='hs-definition'>prop_merge_app</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>ys</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; elems v == Set_cup elems xs elems ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs</span></a> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs' &amp;&amp; elems v == Set_cup elems xs elems ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs'</span></a>
<span class=hs-linenum>355: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>356: </span>    <a class=annot href="#"><span class=annottext>{v : [a] | elems v == Set_cup elems xs elems ys}</span><span class='hs-varid'>zs</span></a>               <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
x1:[a]
-&gt; x2:[a] -&gt; {v : [a] | elems v == Set_cup elems x1 elems x2}</span><span class='hs-varid'>append'</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>357: </span>    <a class=annot href="#"><span class=annottext>{v : [a] | elems v == Set_cup elems xs elems ys}</span><span class='hs-varid'>zs'</span></a>              <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a]
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup elems x2 elems x3}</span><span class='hs-varid'>merge</span></a>   <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
</pre>

\exercisen{MergeSort} \doublestar Once you write the correct type
for `merge` above, you should be able to prove the
surprising signature for `mergeSort` below.


<pre><span class=hs-linenum>365: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>mergeSort</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| Empty v}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>366: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
[a] -&gt; {v : [a] | elems v == Set_empty 0}</span><span class='hs-definition'>mergeSort</span></a> <span class='hs-conid'>[]</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x7 VV -&gt; p x7&gt; | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>367: </span><span class='hs-definition'>mergeSort</span> <span class='hs-varid'>xs</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a]
-&gt; x3:[a] -&gt; {v : [a] | elems v == Set_cup elems x2 elems x3}</span><span class='hs-varid'>merge</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
[a] -&gt; {v : [a] | elems v == Set_empty 0}</span><span class='hs-varid'>mergeSort</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; elems v == Set_cup elems ys elems ys &amp;&amp; elems v == elems ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
[a] -&gt; {v : [a] | elems v == Set_empty 0}</span><span class='hs-varid'>mergeSort</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; elems v == Set_cup elems zs elems zs &amp;&amp; elems v == elems zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>368: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>369: </span>   <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; elems VV == Set_cup elems ys elems ys &amp;&amp; elems VV == elems ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : [a] | VV == zs &amp;&amp; elems VV == Set_cup elems zs elems zs &amp;&amp; elems VV == elems zs &amp;&amp; len VV == len zs &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. GHC.Types.Int -&gt; [a] -&gt; ([a], [a])</span><span class='hs-varid'>split</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == mid}</span><span class='hs-varid'>mid</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>370: </span>   <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>mid</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[a] -&gt; {v : GHC.Types.Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:{v : GHC.Types.Int | v /= 0}
-&gt; {v : GHC.Types.Int | x1 &gt;= 0 &amp;&amp; x2 &gt;= 0 =&gt; v &gt;= 0 &amp;&amp; x1 &gt;= 0 &amp;&amp; x2 &gt;= 1 =&gt; v &lt;= x1 &amp;&amp; v == x1 / x2}</span><span class='hs-varop'>`div`</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a>
</pre>

\noindent First, make sure you are able verify the given
signature. Next, obviously we don't want `mergeSort` to
return the empty list, so there's a bug somewhere in the
code. Find and fix it, so that you *cannot* prove that the
output is empty, but *can* prove that `EqElts v xs`.

Uniqueness
----------

Often, we want to enforce the invariant that a particular collection
contains *no duplicates*; as multiple copies in a collection of file
handles or system resources can create unpleasant leaks.
For example, the [XMonad](xmonad) window manager creates a
sophisticated *zipper* data structure to hold the list of
active user windows, and carefully maintains the invariant
that that there are no duplicates. Next, lets see how to specify
and verify this invariant using LiquidHaskell, first for lists, and
then for a simplified zipper.

\newthought{Specifying Uniqueness} How would we even describe the
fact that a list has no duplicates? There are in fact multiple
different ways, but the simplest is a *measure*:


<pre><span class=hs-linenum>397: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>unique</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>398: </span><span class='hs-definition'>unique</span>        <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>399: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : GHC.Types.Bool | Prop VV &lt;=&gt; unique x2}</span><span class='hs-definition'>unique</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>
<span class=hs-linenum>400: </span><span class='hs-definition'>unique</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : GHC.Types.Bool | Prop VV &lt;=&gt; unique x2}</span><span class='hs-varid'>unique</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; x2:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Prop x1 &amp;&amp; Prop v}</span><span class='hs-varop'>&amp;&amp;</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; not (Prop x1)}</span><span class='hs-varid'>not</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x1 v}</span><span class='hs-varid'>member</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span><span class='hs-layout'>)</span> 
</pre>

\noindent We can define an alias for duplicate-free lists


<pre><span class=hs-linenum>406: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>unique</span> <span class='hs-varid'>v</span> <span class='hs-layout'>}</span><span class='hs-keyword'>@-}</span>
</pre>

\noindent and then do a quick sanity check, that the
right lists are indeed `unique`


<pre><span class=hs-linenum>413: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>isUnique</span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>UList</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>414: </span><a class=annot href="#"><span class=annottext>{v : [GHC.Types.Int] | unique v}</span><span class='hs-definition'>isUnique</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Int] | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a><span class='hs-keyglyph'>]</span>        <span class='hs-comment'>-- accepted by LH</span>
<span class=hs-linenum>415: </span>
<span class=hs-linenum>416: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>isNotUnique</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>UList</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>417: </span><a class=annot href="#"><span class=annottext>{v : [GHC.Types.Int] | unique v}</span><span class='hs-definition'>isNotUnique</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [GHC.Types.Int] | null v &lt;=&gt; false &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a></span><span class=hs-error><span class='hs-layout'>,</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error><span class='hs-keyglyph'>]</span></span>     <span class='hs-comment'>-- rejected by LH</span>
</pre>

\newthought{Filter}  Lets write some functions that preserve
`unique`ness. For example, `filter` returns a subset of its
elements. Hence, if the input was unique, the output is too:


<pre><span class=hs-linenum>425: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>filter</span>      <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>| SubElts v xs}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>426: </span><a class=annot href="#"><span class=annottext>forall a.
(a -&gt; GHC.Types.Bool)
-&gt; x3:{v : [a] | unique v}
-&gt; {v : [a] | Set_sub elems v elems x3 &amp;&amp; unique v}</span><span class='hs-definition'>filter</span></a> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x7 VV -&gt; p x7&gt; | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>427: </span><span class='hs-definition'>filter</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>428: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>a -&gt; GHC.Types.Bool</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | Set_sub elems v elems xs &amp;&amp; unique v &amp;&amp; v == xs' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs'</span></a> 
<span class=hs-linenum>429: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | Set_sub elems v elems xs &amp;&amp; unique v &amp;&amp; v == xs' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs'</span></a> 
<span class=hs-linenum>430: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>431: </span>    <a class=annot href="#"><span class=annottext>{v : [a] | Set_sub elems v elems xs &amp;&amp; unique v}</span><span class='hs-varid'>xs'</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
(a -&gt; GHC.Types.Bool)
-&gt; x3:{v : [a] | unique v}
-&gt; {v : [a] | Set_sub elems v elems x3 &amp;&amp; unique v}</span><span class='hs-varid'>filter</span></a> <a class=annot href="#"><span class=annottext>a -&gt; GHC.Types.Bool</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\exercisen{Reverse} \singlestar 
When we `reverse` their order, the set of elements is unchanged,
and hence unique (if the input was unique). Why does LiquidHaskell
reject the below? Can you fix things so that we can prove that the
output is a `UList a`?


<pre><span class=hs-linenum>441: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>reverse</span>     <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>442: </span><a class=annot href="#"><span class=annottext>forall a. {v : [a] | unique v} -&gt; {v : [a] | unique v}</span><span class='hs-definition'>reverse</span></a>         <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a></span>
<span class=hs-linenum>443: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>444: </span>    <span class='hs-keyword'>{-@</span> <span class='hs-varid'>go</span>      <span class='hs-keyglyph'>::</span> <span class='hs-varid'>acc</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>445: </span>    <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>a</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == a &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>a</span></a>
<span class=hs-linenum>446: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == a &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>a</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> 
</pre>

\newthought{Nub} One way to create a `unique` list is to start
with an ordinary list and throw away elements that we have `seen`
already.


<pre><span class=hs-linenum>454: </span><a class=annot href="#"><span class=annottext>forall a. (GHC.Classes.Eq a) =&gt; [a] -&gt; [a]</span><span class='hs-definition'>nub</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>xs</span></a>                <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a> 
<span class=hs-linenum>455: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>456: </span>    <a class=annot href="#"><span class=annottext>forall a. (GHC.Classes.Eq a) =&gt; [a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>seen</span></a> <span class='hs-conid'>[]</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == seen &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>seen</span></a>
<span class=hs-linenum>457: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>seen</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>458: </span>      <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Eq a) =&gt;
x2:a
-&gt; x3:[a] -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x2 elems x3}</span><span class='hs-varop'>`isin`</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == seen &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>seen</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == seen &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>seen</span></a>     <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>459: </span>      <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; [a]</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == seen &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>seen</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\noindent The key membership test is done by `isin`,
whose output is `True` exactly when the element is
in the given list. \footnotetext{Which should be
clear by now, if you did the exercise above \ldots}


<pre><span class=hs-linenum>468: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>isin</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>ys</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v &lt;=&gt; Elem x ys }</span><span class='hs-keyword'>@-}</span>
<span class=hs-linenum>469: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Eq a) =&gt;
x2:a
-&gt; x3:[a] -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x2 elems x3}</span><span class='hs-definition'>isin</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>y</span><span class='hs-conop'>:</span><span class='hs-varid'>ys</span><span class='hs-layout'>)</span>
<span class=hs-linenum>470: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>
<span class=hs-linenum>471: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Eq a) =&gt;
x2:a
-&gt; x3:[a] -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x2 elems x3}</span><span class='hs-varop'>`isin`</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>472: </span><span class='hs-definition'>isin</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
</pre>

\exercisen{Append} \singlestar Why does appending two
`UList`s not return a `UList`? Fix the type signature
below so that you can prove that the output is indeed
`unique`.


<pre><span class=hs-linenum>481: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>append</span>       <span class='hs-keyglyph'>::</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>482: </span><a class=annot href="#"><span class=annottext>forall a.
{v : [a] | unique v}
-&gt; {v : [a] | unique v} -&gt; {v : [a] | unique v}</span><span class='hs-definition'>append</span></a> <span class='hs-conid'>[]</span>     <a class=annot href="#"><span class=annottext>{v : [a] | unique v}</span><span class='hs-varid'>ys</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | unique v &amp;&amp; v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>483: </span><span class='hs-definition'>append</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varid'>ys</span> <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a.
{v : [a] | unique v}
-&gt; {v : [a] | unique v} -&gt; {v : [a] | unique v}</span><span class='hs-varid'>append</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | unique v &amp;&amp; v == ys &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ys</span></a></span>
</pre>

\exercisen{Range} \doublestar In the below `range i j`
returns the list of all `Int` between `i` and `j`.
Yet, LiquidHaskell refuses to acknowledge that the
output is indeed a `UList`. Modify the specification
and implementation, if needed, to obtain an equivalent
of `range` which *provably* returns a `UList Int`.


<pre><span class=hs-linenum>494: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Btwn</span> <span class='hs-conid'>I</span> <span class='hs-conid'>J</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>I</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;</span> <span class='hs-conid'>J</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>495: </span>                   
<span class=hs-linenum>496: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>range</span>     <span class='hs-keyglyph'>::</span> <span class='hs-varid'>i</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>j</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>UList</span> <span class='hs-layout'>(</span><span class='hs-conid'>Btwn</span> <span class='hs-varid'>i</span> <span class='hs-varid'>j</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>497: </span><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int
-&gt; {v : [{v : GHC.Types.Int | v &lt; x2 &amp;&amp; x1 &lt;= v}] | unique v}</span><span class='hs-definition'>range</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>j</span></a>
<span class=hs-linenum>498: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == i}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == j}</span><span class='hs-varid'>j</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == i}</span><span class='hs-varid'>i</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : GHC.Types.Int | v &gt;= i &amp;&amp; v &lt; j}
-&gt; x2:[{v : GHC.Types.Int | v &gt; x1 &amp;&amp; v &gt; i &amp;&amp; v &gt;= i &amp;&amp; v &lt; j}]&lt;\x20 VV -&gt; v &gt; x20 &amp;&amp; v &gt; i &amp;&amp; v &lt; j&gt;
-&gt; {v : [{v : GHC.Types.Int | v &gt;= i &amp;&amp; v &lt; j}]&lt;\x11 VV -&gt; v &gt; x11 &amp;&amp; v &gt; i &amp;&amp; v &lt; j&gt; | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int
-&gt; {v : [{v : GHC.Types.Int | v &lt; x2 &amp;&amp; x1 &lt;= v}] | unique v}</span><span class='hs-varid'>range</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-layout'>(</span></span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == i}</span><span class='hs-varid'>i</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == j}</span><span class='hs-varid'>j</span></a></span>
<span class=hs-linenum>499: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [{v : GHC.Types.Int | false}] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a> 
</pre>

Unique Zippers
--------------

A [zipper](wiki-zipper) is an aggregate data stucture 
that is used to arbitrarily traverse the structure and
update its contents. For example, a zipper for a list is
a data type that contains an element (called `focus`)
that we are currently `focus`-ed on, a list of elements
to the `left` of (i.e. before) the focus, and a list of
elements to the `right` (i.e. after) the focus.



<pre><span class=hs-linenum>515: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Zipper</span> <span class='hs-layout'>{</span>
<span class=hs-linenum>516: </span>    <a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; a</span><span class='hs-varid'>focus</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span>      
<span class=hs-linenum>517: </span>  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; [a]</span><span class='hs-varid'>left</span></a>   <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>    
<span class=hs-linenum>518: </span>  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; [a]</span><span class='hs-varid'>right</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>519: </span>  <span class='hs-layout'>}</span>  
</pre>

\newthought{XMonad} is a wonderful tiling window manager, that uses
a [zipper](xmonad-stackset) to store the set of windows being managed.
Xmonad requires the crucial invariant that the values in the zipper
be unique, i.e. have no duplicates.

\newthought{Refined Zipper}  
We can specify that all the values in the zipper are unique
by refining the `Zipper` data declaration to express that
both the lists in the structure are unique, disjoint,
and do not include `focus`.


<pre><span class=hs-linenum>534: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Zipper</span> <span class='hs-layout'>{</span>
<span class=hs-linenum>535: </span>      <span class='hs-varid'>focus</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>536: </span>    <span class='hs-layout'>,</span> <span class='hs-varid'>left</span>  <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>not</span> <span class='hs-layout'>(</span><span class='hs-conid'>Elem</span> <span class='hs-varid'>focus</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span><span class='hs-layout'>}</span>
<span class=hs-linenum>537: </span>    <span class='hs-layout'>,</span> <span class='hs-varid'>right</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>not</span> <span class='hs-layout'>(</span><span class='hs-conid'>Elem</span> <span class='hs-varid'>focus</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varop'>&amp;&amp;</span> <span class='hs-conid'>DisjElts</span> <span class='hs-varid'>v</span> <span class='hs-varid'>left</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>538: </span>    <span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\newthought{Constructing Zippers}
Our refined type makes *illegal states unrepresentable*;
by construction, we will ensure that every `Zipper` is
free of duplicates. Of course, it is straightforward to
create a valid `Zipper` from a `unique` list:


<pre><span class=hs-linenum>548: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>differentiate</span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>549: </span><a class=annot href="#"><span class=annottext>forall a.
{v : [a] | unique v} -&gt; (Data.Maybe.Maybe (Sets.Zipper a))</span><span class='hs-definition'>differentiate</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Data.Maybe.Maybe a) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a>
<span class=hs-linenum>550: </span><span class='hs-definition'>differentiate</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Sets.Zipper a)
-&gt; {v : (Data.Maybe.Maybe (Sets.Zipper a)) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <a class=annot href="#"><span class=annottext>((Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a)))
-&gt; (Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a))</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v}
-&gt; x3:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v &amp;&amp; Set_empty 0 == Set_cap elems v elems x2}
-&gt; {v : (Sets.Zipper a) | left v == x2 &amp;&amp; right v == x3 &amp;&amp; focus v == x1}</span><span class='hs-conid'>Zipper</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

\exercisen{Deconstructing Zippers} \singlestar
\noindent Dually, the elements of a unique zipper tumble out
into a unique list. Strengthen the types of `reverse` and `append`
above so that LiquidHaskell accepts the below signatures for `integrate`:


<pre><span class=hs-linenum>559: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>integrate</span>            <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>UList</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>560: </span><a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; {v : [a] | unique v}</span><span class='hs-definition'>integrate</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>x</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | unique v} -&gt; {v : [a] | unique v}</span><span class='hs-varid'>reverse</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem x elems v) &amp;&amp; unique v &amp;&amp; v == l &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>forall a.
{v : [a] | unique v}
-&gt; {v : [a] | unique v} -&gt; {v : [a] | unique v}</span><span class='hs-varop'>`append`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem x elems v) &amp;&amp; unique v &amp;&amp; v == r &amp;&amp; Set_empty 0 == Set_cap elems v elems l &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>r</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{Shifting Focus} We can shift the focus element
left or right while preserving the invariants:


<pre><span class=hs-linenum>567: </span><span class='hs-definition'>focusLeft</span>                      <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>568: </span><a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; (Sets.Zipper a)</span><span class='hs-definition'>focusLeft</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>t</span> <span class='hs-conid'>[]</span> <span class='hs-varid'>rs</span><span class='hs-layout'>)</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v}
-&gt; x3:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v &amp;&amp; Set_empty 0 == Set_cap elems v elems x2}
-&gt; {v : (Sets.Zipper a) | left v == x2 &amp;&amp; right v == x3 &amp;&amp; focus v == x1}</span><span class='hs-conid'>Zipper</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x &amp;&amp; VV == x}</span><span class='hs-varid'>x</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [a] | unique v &amp;&amp; v == xs &amp;&amp; v == xs &amp;&amp; elems v == Set_cup elems xs elems xs &amp;&amp; elems v == elems xs &amp;&amp; len v == len xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span> <a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a>     <span class='hs-keyword'>where</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-conop'>:</span><a class=annot href="#"><span class=annottext>{VV : [a] | unique VV &amp;&amp; VV == xs &amp;&amp; elems VV == Set_cup elems xs elems xs &amp;&amp; elems VV == elems xs &amp;&amp; len VV == len xs &amp;&amp; len VV &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | unique v} -&gt; {v : [a] | unique v}</span><span class='hs-varid'>reverse</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == t}</span><span class='hs-varid'>t</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem t elems v) &amp;&amp; unique v &amp;&amp; v == rs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>rs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>569: </span><span class='hs-definition'>focusLeft</span> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>t</span> <span class='hs-layout'>(</span><span class='hs-varid'>l</span><span class='hs-conop'>:</span><span class='hs-varid'>ls</span><span class='hs-layout'>)</span> <span class='hs-varid'>rs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v}
-&gt; x3:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v &amp;&amp; Set_empty 0 == Set_cap elems v elems x2}
-&gt; {v : (Sets.Zipper a) | left v == x2 &amp;&amp; right v == x3 &amp;&amp; focus v == x1}</span><span class='hs-conid'>Zipper</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == l}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ls &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ls</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == t}</span><span class='hs-varid'>t</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem t elems v) &amp;&amp; unique v &amp;&amp; v == rs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>rs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>570: </span>
<span class=hs-linenum>571: </span><span class='hs-definition'>focusRight</span>                     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>572: </span><a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; (Sets.Zipper a)</span><span class='hs-definition'>focusRight</span></a>                     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Sets.Zipper a) -&gt; (Sets.Zipper a)</span><span class='hs-varid'>reverseZipper</span></a> <a class=annot href="#"><span class=annottext>((Sets.Zipper a) -&gt; (Sets.Zipper a))
-&gt; ((Sets.Zipper a) -&gt; (Sets.Zipper a))
-&gt; (Sets.Zipper a)
-&gt; exists [(Sets.Zipper a)].(Sets.Zipper a)</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>(Sets.Zipper a) -&gt; (Sets.Zipper a)</span><span class='hs-varid'>focusLeft</span></a> <a class=annot href="#"><span class=annottext>((Sets.Zipper a) -&gt; (Sets.Zipper a))
-&gt; ((Sets.Zipper a) -&gt; (Sets.Zipper a))
-&gt; (Sets.Zipper a)
-&gt; exists [(Sets.Zipper a)].(Sets.Zipper a)</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>(Sets.Zipper a) -&gt; (Sets.Zipper a)</span><span class='hs-varid'>reverseZipper</span></a>
<span class=hs-linenum>573: </span>
<span class=hs-linenum>574: </span><span class='hs-definition'>reverseZipper</span>                  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>575: </span><a class=annot href="#"><span class=annottext>forall a. (Sets.Zipper a) -&gt; (Sets.Zipper a)</span><span class='hs-definition'>reverseZipper</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>t</span> <span class='hs-varid'>ls</span> <span class='hs-varid'>rs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v}
-&gt; x3:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v &amp;&amp; Set_empty 0 == Set_cap elems v elems x2}
-&gt; {v : (Sets.Zipper a) | left v == x2 &amp;&amp; right v == x3 &amp;&amp; focus v == x1}</span><span class='hs-conid'>Zipper</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == t}</span><span class='hs-varid'>t</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem t elems v) &amp;&amp; unique v &amp;&amp; v == rs &amp;&amp; Set_empty 0 == Set_cap elems v elems ls &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>rs</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem t elems v) &amp;&amp; unique v &amp;&amp; v == ls &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ls</span></a>
</pre>

\newthought{Filter} Finally, using the filter operation on lists
allows LiquidHaskell to prove that filtering a zipper 
also preserves uniqueness.

<pre><span class=hs-linenum>582: </span><span class='hs-definition'>filterZipper</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>583: </span><a class=annot href="#"><span class=annottext>forall a.
(a -&gt; GHC.Types.Bool)
-&gt; (Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a))</span><span class='hs-definition'>filterZipper</span></a> <a class=annot href="#"><span class=annottext>a -&gt; GHC.Types.Bool</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Zipper</span> <span class='hs-varid'>f</span> <span class='hs-varid'>ls</span> <span class='hs-varid'>rs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>case</span> <a class=annot href="#"><span class=annottext>forall a.
(a -&gt; GHC.Types.Bool)
-&gt; x3:{v : [a] | unique v}
-&gt; {v : [a] | Set_sub elems v elems x3 &amp;&amp; unique v}</span><span class='hs-varid'>filter</span></a> <a class=annot href="#"><span class=annottext>a -&gt; GHC.Types.Bool</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == f}</span><span class='hs-varid'>f</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | unique v &lt;=&gt; unique x2 &amp;&amp; not (Set_mem x1 elems x2) &amp;&amp; null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; elems v == Set_cup Set_sng x1 elems x2 &amp;&amp; listElts v == Set_cup Set_sng x1 listElts x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; len v == 1 + len x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem f elems v) &amp;&amp; unique v &amp;&amp; v == rs &amp;&amp; Set_empty 0 == Set_cap elems v elems ls &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>rs</span></a><span class='hs-layout'>)</span> <span class='hs-keyword'>of</span>
<span class=hs-linenum>584: </span>    <span class='hs-varid'>f'</span><span class='hs-conop'>:</span><span class='hs-varid'>rs'</span> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:(Sets.Zipper a)
-&gt; {v : (Data.Maybe.Maybe (Sets.Zipper a)) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <a class=annot href="#"><span class=annottext>((Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a)))
-&gt; (Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a))</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v}
-&gt; x3:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v &amp;&amp; Set_empty 0 == Set_cap elems v elems x2}
-&gt; {v : (Sets.Zipper a) | left v == x2 &amp;&amp; right v == x3 &amp;&amp; focus v == x1}</span><span class='hs-conid'>Zipper</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == f'}</span><span class='hs-varid'>f'</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a.
(a -&gt; GHC.Types.Bool)
-&gt; x3:{v : [a] | unique v}
-&gt; {v : [a] | Set_sub elems v elems x3 &amp;&amp; unique v}</span><span class='hs-varid'>filter</span></a> <a class=annot href="#"><span class=annottext>a -&gt; GHC.Types.Bool</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem f elems v) &amp;&amp; unique v &amp;&amp; v == ls &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ls</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == rs' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>rs'</span></a>      <span class='hs-comment'>-- maybe move focus right </span>
<span class=hs-linenum>585: </span>    <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>case</span> <a class=annot href="#"><span class=annottext>forall a.
(a -&gt; GHC.Types.Bool)
-&gt; x3:{v : [a] | unique v}
-&gt; {v : [a] | Set_sub elems v elems x3 &amp;&amp; unique v}</span><span class='hs-varid'>filter</span></a> <a class=annot href="#"><span class=annottext>a -&gt; GHC.Types.Bool</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | not (Set_mem f elems v) &amp;&amp; unique v &amp;&amp; v == ls &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ls</span></a> <span class='hs-keyword'>of</span>                     <span class='hs-comment'>-- filter back left</span>
<span class=hs-linenum>586: </span>                    <span class='hs-varid'>f'</span><span class='hs-conop'>:</span><span class='hs-varid'>ls'</span> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:(Sets.Zipper a)
-&gt; {v : (Data.Maybe.Maybe (Sets.Zipper a)) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <a class=annot href="#"><span class=annottext>((Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a)))
-&gt; (Sets.Zipper a) -&gt; (Data.Maybe.Maybe (Sets.Zipper a))</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v}
-&gt; x3:{v : [a] | not (Set_mem x1 elems v) &amp;&amp; unique v &amp;&amp; Set_empty 0 == Set_cap elems v elems x2}
-&gt; {v : (Sets.Zipper a) | left v == x2 &amp;&amp; right v == x3 &amp;&amp; focus v == x1}</span><span class='hs-conid'>Zipper</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == f'}</span><span class='hs-varid'>f'</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ls' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ls'</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | unique v &lt;=&gt; true &amp;&amp; null v &lt;=&gt; true &amp;&amp; Set_emp listElts v &amp;&amp; elems v == Set_empty 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a> <span class='hs-comment'>-- else left</span>
<span class=hs-linenum>587: </span>                    <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Data.Maybe.Maybe a) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a>
</pre>

Recap
-----

In this chapter, we saw how SMT solvers can let us reason precisely about
the actual *contents* of data structures, via the theory of sets. We can

* Lift the set-theoretic primitives to (refined) Haskell functions from
  the `Data.Set` library,

* Use the functions to define measures like `elems` that characterize
  the contents of structures, and `unique` that describe high-level
  application specific properties.

* Use LiquidHaskell to then specify and verify that implementations
  enjoy various functional correctness properties, e.g. that sorting
  routines return permutations of their inputs, and various zipper
  operators preserve uniqueness.

Next, we present a variety of *case-studies* illustrating the techniques
so far on particular application domains.


