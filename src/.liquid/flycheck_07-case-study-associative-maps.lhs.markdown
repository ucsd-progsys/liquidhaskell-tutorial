
Case Study: Associative Maps
============================

\begin{comment}

<pre><span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span>
<span class=hs-linenum>10: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>AssocativeMap</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>11: </span>
<span class=hs-linenum>12: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Set</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span><span class='hs-layout'>)</span>
<span class=hs-linenum>13: </span><span class='hs-comment'>-- | Boilerplate </span>
<span class=hs-linenum>14: </span>
<span class=hs-linenum>15: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>die</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| false}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>16: </span><a class=annot href="#"><span class=annottext>forall a. {v : [GHC.Types.Char] | false} -&gt; a</span><span class='hs-definition'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false}</span><span class='hs-varid'>x</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[GHC.Types.Char] -&gt; a</span><span class='hs-varid'>error</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false}</span><span class='hs-varid'>x</span></a>
<span class=hs-linenum>17: </span>
<span class=hs-linenum>18: </span><span class='hs-comment'>-- | Haskell Definitions</span>
<span class=hs-linenum>19: </span>
<span class=hs-linenum>20: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Node</span> <span class='hs-layout'>{</span> <a class=annot href="#"><span class=annottext>forall a b. (AssocativeMap.Map a b) -&gt; a</span><span class='hs-varid'>key</span></a>   <span class='hs-keyglyph'>::</span> <span class='hs-varid'>k</span>
<span class=hs-linenum>21: </span>                    <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a b. (AssocativeMap.Map a b) -&gt; b</span><span class='hs-varid'>value</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>22: </span>                    <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a b. (AssocativeMap.Map a b) -&gt; (AssocativeMap.Map a b)</span><span class='hs-varid'>left</span></a>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>23: </span>                    <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>forall a b. (AssocativeMap.Map a b) -&gt; (AssocativeMap.Map a b)</span><span class='hs-varid'>right</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>24: </span>             <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Tip</span> 
<span class=hs-linenum>25: </span>
<span class=hs-linenum>26: </span><span class='hs-definition'>lemma_notMem</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span> 
<span class=hs-linenum>27: </span><span class='hs-definition'>set</span>    <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>28: </span><span class='hs-definition'>get</span>    <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> 
<span class=hs-linenum>29: </span><span class='hs-definition'>get'</span>   <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> 
<span class=hs-linenum>30: </span><span class='hs-definition'>mem</span>    <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>31: </span><span class='hs-definition'>emp</span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>32: </span><span class='hs-definition'>elems</span>  <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>33: </span><span class='hs-definition'>fresh</span>  <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>34: </span><span class='hs-comment'>-- | Predicate Aliases</span>
<span class=hs-linenum>35: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>NoKey</span> <span class='hs-conid'>M</span>       <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Empty</span> <span class='hs-layout'>(</span><span class='hs-varid'>keys</span> <span class='hs-conid'>M</span><span class='hs-layout'>)</span>                        <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>36: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>HasKey</span> <span class='hs-conid'>K</span> <span class='hs-conid'>M</span>    <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_mem</span> <span class='hs-conid'>K</span> <span class='hs-layout'>(</span><span class='hs-varid'>keys</span> <span class='hs-conid'>M</span><span class='hs-layout'>)</span>                    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>37: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>PlusKey</span> <span class='hs-conid'>K</span> <span class='hs-conid'>M</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>keys</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_cup</span> <span class='hs-layout'>(</span><span class='hs-conid'>Set_sng</span> <span class='hs-conid'>K</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>keys</span> <span class='hs-conid'>M</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>38: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>Subset</span> <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span>    <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_sub</span> <span class='hs-conid'>X</span> <span class='hs-conid'>Y</span>                           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>39: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>Empty</span>  <span class='hs-conid'>X</span>      <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_emp</span> <span class='hs-conid'>X</span>                             <span class='hs-keyword'>@-}</span>
</pre>
\end{comment}

\begin{comment}
\begin{figure}[h]
\includegraphics[height=1.0in]{img/piponi-tweet.png}
\caption{Wouldn't it be nice to know that a key was in a map?} 
\label{fig:piponi-tweet}
\end{figure}
\end{comment}

Recall the following from the [introduction](#intro).

\begin{ghci}
ghci> :m +Data.Map 
ghci> let m = fromList [ ("haskell"   , "lazy")
                       , ("javascript", "eager")]

ghci> m ! "haskell"
"lazy"

ghci> m ! "python"
"*** Exception: key is not in the map
\end{ghci}

\noindent
The problem illustrated above is quite a pervasive one; associative
maps pop up everywhere. Failed lookups are the equivalent of
`NullPointerDereference` exceptions in languages like Haskell.
It is rather difficult to use Haskell's type system to precisely
characterize the behavior of associative map APIs as ultimately,
this requires tracking the *dynamic set of keys* in the map.

In this case study, we'll see how to combine two techniques -- 
[measures](#setmeasure) for reasoning about the *sets* of elements
in structures, and [refined data types](#refineddatatypes) for
reasoning about order invariants -- can be applied to programs
that use associative maps (e.g. `Data.Map` or `Data.HashMap`).

Specifying Maps {#mapapi}
-------------------------

Lets start by defining a *refined API* for Associative Maps
that tracks the set of keys stored in the map, in order to
statically ensure the safety of lookups.

\newthought{Types} First, we need an (currently abstract)
type for `Map`s. As usual, lets parameterize the type with
`k` for the type of keys and `v` for the type of values.


<pre><span class=hs-linenum>91: </span><span class='hs-comment'>-- | Data Type</span>
<span class=hs-linenum>92: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span>
</pre>

\newthought{Keys} To talk about the set of keys in a map,
we will use a *measure*


<pre><span class=hs-linenum>99: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>keys</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>k</span>
</pre>

\noindent that associates each `Map` to the `Set` of its
defined keys. Next, we use the above measure, and the usual
`Set` operators to refine the types of the functions that
*create*, *add* and *lookup* key-value bindings, in order
to precisely track, within the type system, the `keys`
that are dynamically defined within each `Map`.

\newthought{Empty} `Map`s have no keys in them. Hence, we
defined a predicate alias, `NoKey` and use it to type `emp`
which is used to denote the empty `Map`:


<pre><span class=hs-linenum>114: </span><span class='hs-definition'>emp</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>NoKey</span> <span class='hs-varid'>m</span><span class='hs-layout'>}</span>
<span class=hs-linenum>115: </span>
<span class=hs-linenum>116: </span><span class='hs-definition'>predicate</span> <span class='hs-conid'>NoKey</span> <span class='hs-conid'>M</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>keys</span> <span class='hs-conid'>M</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_empty</span> <span class='hs-num'>0</span>
</pre>

\newthought{Add} The function `set` takes a key $k$ a
value $v$ and a map `m` and returns the new map obtained
by extending `m` with the binding ${k \mapsto v}$.
Thus, the set of `keys` of the output `Map` includes
those of the input plus the singleton $k$, that is:


<pre><span class=hs-linenum>126: </span><span class='hs-definition'>set</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>n</span><span class='hs-conop'>:</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>PlusKey</span> <span class='hs-varid'>k</span> <span class='hs-varid'>m</span> <span class='hs-varid'>n</span><span class='hs-layout'>}</span>
<span class=hs-linenum>127: </span>
<span class=hs-linenum>128: </span><span class='hs-definition'>predicate</span> <span class='hs-conid'>PlusKey</span> <span class='hs-conid'>K</span> <span class='hs-conid'>M</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>keys</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_cup</span> <span class='hs-layout'>(</span><span class='hs-conid'>Set_sng</span> <span class='hs-conid'>K</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>keys</span> <span class='hs-conid'>M</span><span class='hs-layout'>)</span>
</pre>

\newthought{Query} Finally, queries will only succeed for keys that are defined
a given `Map`. Thus, we define an alias:


<pre><span class=hs-linenum>135: </span><span class='hs-definition'>predicate</span> <span class='hs-conid'>HasKey</span> <span class='hs-conid'>K</span> <span class='hs-conid'>M</span>    <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_mem</span> <span class='hs-conid'>K</span> <span class='hs-layout'>(</span><span class='hs-varid'>keys</span> <span class='hs-conid'>M</span><span class='hs-layout'>)</span>
</pre>

\noindent and use it to type `mem` which *checks* if
a key is defined in the `Map` and `get` which actually
returns the value associated with a given key.


<pre><span class=hs-linenum>143: </span><span class='hs-comment'>-- | Check if key is defined </span>
<span class=hs-linenum>144: </span><span class='hs-definition'>mem</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Bool</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Prop</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;=&gt;</span> <span class='hs-conid'>HasKey</span> <span class='hs-varid'>k</span> <span class='hs-varid'>m</span><span class='hs-layout'>}</span>
<span class=hs-linenum>145: </span>
<span class=hs-linenum>146: </span><span class='hs-comment'>-- | Lookup key's value </span>
<span class=hs-linenum>147: </span><span class='hs-definition'>get</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>HasKey</span> <span class='hs-varid'>k</span> <span class='hs-varid'>m</span><span class='hs-layout'>}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span>
</pre>

Using Maps: Well Scoped Expressions 
----------------------------------- 

Rather than jumping into the *implementation* of the above `Map` API,
lets write a *client* that uses `Map`s to implement an interpreter for
a tiny language. In particular, we will use maps as an *environment*
containing the values of *bound variables*, and we will use the refined
API to ensure that *lookups never fail*, and hence, that well-scoped
programs always reduce to a value.

\newthought{Expressions} Lets work with a simple language with
integer constants, variables, binding and arithmetic operators:
\footnotetext{Feel free to embellish the language with fancier
features like functions, tuples etc.}


<pre><span class=hs-linenum>166: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>Var</span>  <span class='hs-keyglyph'>=</span> <span class='hs-conid'>String</span> 
<span class=hs-linenum>167: </span>
<span class=hs-linenum>168: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Const</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>169: </span>          <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Var</span>   <span class='hs-conid'>Var</span>
<span class=hs-linenum>170: </span>          <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Plus</span>  <span class='hs-conid'>Expr</span> <span class='hs-conid'>Expr</span>
<span class=hs-linenum>171: </span>          <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Let</span>   <span class='hs-conid'>Var</span>  <span class='hs-conid'>Expr</span> <span class='hs-conid'>Expr</span>
</pre>

\newthought{Values} We can use refinements to formally
describe *values* as a subset of `Expr` allowing us to
reuse a bunch of code. To this end, we simply define a
(`measure`) predicate characterizing values:


<pre><span class=hs-linenum>180: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>val</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>181: </span><span class='hs-definition'>val</span>              <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span> 
<span class=hs-linenum>182: </span><a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr -&gt; {VV : GHC.Types.Bool | Prop VV &lt;=&gt; val x1}</span><span class='hs-definition'>val</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a> 
<span class=hs-linenum>183: </span><span class='hs-definition'>val</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a> 
<span class=hs-linenum>184: </span><span class='hs-definition'>val</span> <span class='hs-layout'>(</span><span class='hs-conid'>Plus</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a> 
<span class=hs-linenum>185: </span><span class='hs-definition'>val</span> <span class='hs-layout'>(</span><span class='hs-conid'>Let</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a> 
</pre>

\noindent and then we can use the lifted `measure` to
define an alias for `Val` denoting values:


<pre><span class=hs-linenum>192: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Val</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>val</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent we can use the above to write simple *operators*
on `Val`, for example:


<pre><span class=hs-linenum>199: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>plus</span>                 <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Val</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Val</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Val</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>200: </span><a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v}
-&gt; {v : AssocativeMap.Expr | val v}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-definition'>plus</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-varid'>j</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int
-&gt; {v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; free v == Set_empty 0}</span><span class='hs-conid'>Const</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == i}</span><span class='hs-varid'>i</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == j}</span><span class='hs-varid'>j</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>201: </span><span class='hs-definition'>plus</span> <span class='hs-keyword'>_</span>         <span class='hs-keyword'>_</span>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false} -&gt; {v : AssocativeMap.Expr | false}</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | len v &gt;= 0}</span><span class='hs-str'>"Bad call to plus"</span></a>
</pre>

\newthought{Environments} let us save values for the
"local" i.e. *let-bound* variables; when evaluating
an expression `Var x` we simply look up the value of
`x` in the environment. This is why `Map`s were
invented! Lets define our environments as `Map`s from `Var`iables to `Val`ues:


<pre><span class=hs-linenum>211: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Env</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Map</span> <span class='hs-conid'>Var</span> <span class='hs-conid'>Val</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent The above definition essentially specifies, inside the
types, an *eager* evaluation strategy: LiquidHaskell will prevent
us from sticking unevaluated `Expr`s inside the environments.

\newthought{Evaluation} proceeds via a straightforward recursion
over the structure of the expression. When we hit a `Var` we simply
query its value from the environment. When we hit a `Let` we compute
the bound expression and tuck its value into the environment before
proceeding within.


<pre><span class=hs-linenum>225: </span><a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})
-&gt; {v : AssocativeMap.Expr | Set_sub free v keys x1}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-definition'>eval</span></a> <span class='hs-keyword'>_</span> <a class=annot href="#"><span class=annottext>AssocativeMap.Expr</span><span class='hs-varid'>i</span></a><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; v == i &amp;&amp; free v == Set_empty 0}</span><span class='hs-varid'>i</span></a>
<span class=hs-linenum>226: </span><span class='hs-definition'>eval</span> <span class='hs-varid'>g</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-varid'>get</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})</span><span class='hs-varid'>g</span></a> 
<span class=hs-linenum>227: </span><span class='hs-definition'>eval</span> <span class='hs-varid'>g</span> <span class='hs-layout'>(</span><span class='hs-conid'>Plus</span> <span class='hs-varid'>e1</span> <span class='hs-varid'>e2</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v}
-&gt; {v : AssocativeMap.Expr | val v}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>plus</span></a>  <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {VV : AssocativeMap.Expr | val VV})
-&gt; {VV : AssocativeMap.Expr | Set_sub free VV keys x1}
-&gt; {VV : AssocativeMap.Expr | val VV}</span><span class='hs-varid'>eval</span></a> <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e1}</span><span class='hs-varid'>e1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {VV : AssocativeMap.Expr | val VV})
-&gt; {VV : AssocativeMap.Expr | Set_sub free VV keys x1}
-&gt; {VV : AssocativeMap.Expr | val VV}</span><span class='hs-varid'>eval</span></a> <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e2}</span><span class='hs-varid'>e2</span></a><span class='hs-layout'>)</span> 
<span class=hs-linenum>228: </span><span class='hs-definition'>eval</span> <span class='hs-varid'>g</span> <span class='hs-layout'>(</span><span class='hs-conid'>Let</span> <span class='hs-varid'>x</span> <span class='hs-varid'>e1</span> <span class='hs-varid'>e2</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {VV : AssocativeMap.Expr | val VV})
-&gt; {VV : AssocativeMap.Expr | Set_sub free VV keys x1}
-&gt; {VV : AssocativeMap.Expr | val VV}</span><span class='hs-varid'>eval</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v &amp;&amp; v /= i}) | v == g'}</span><span class='hs-varid'>g'</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e2}</span><span class='hs-varid'>e2</span></a> 
<span class=hs-linenum>229: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>230: </span>    <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v &amp;&amp; v /= i})</span><span class='hs-varid'>g'</span></a>               <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; b
-&gt; x4:(AssocativeMap.Map a b)
-&gt; {n : (AssocativeMap.Map a b) | keys n == Set_cup Set_sng x2 keys x4}</span><span class='hs-varid'>set</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &amp;&amp; v == v1}</span><span class='hs-varid'>v1</span></a> <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})</span><span class='hs-varid'>g</span></a> 
<span class=hs-linenum>231: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>v1</span></a>               <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {VV : AssocativeMap.Expr | val VV})
-&gt; {VV : AssocativeMap.Expr | Set_sub free VV keys x1}
-&gt; {VV : AssocativeMap.Expr | val VV}</span><span class='hs-varid'>eval</span></a> <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e1}</span><span class='hs-varid'>e1</span></a>
</pre>

The above `eval` seems rather unsafe; whats the guarantee that
`get x g` will succeed? For example, surely trying:

\begin{ghci}
ghci> eval emp (Var "x")
\end{ghci}

\noindent will lead to some unpleasant crash. Shouldn't we *check*
if the variables is present and if not, fail with some sort of
`Variable Not Bound` error? We could, but we can do better: we can
prove at compile time, that such errors will not occur.

\newthought{Free Variables} are those whose values are *not* bound
within an expression, that is, the set of variables that *appear* in
the expression, but are not *bound* by a dominating `Let`. We can
formalize this notion as a (lifted) function:


<pre><span class=hs-linenum>252: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>free</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>253: </span><span class='hs-definition'>free</span>               <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Set</span> <span class='hs-conid'>Var</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>254: </span><a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr
-&gt; {VV : (Data.Set.Base.Set [GHC.Types.Char]) | VV == free x1}</span><span class='hs-definition'>free</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Const</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Data.Set.Base.Set a) | Set_emp v}</span><span class='hs-varid'>empty</span></a>
<span class=hs-linenum>255: </span><span class='hs-definition'>free</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[GHC.Types.Char]
-&gt; {v : (Data.Set.Base.Set [GHC.Types.Char]) | v == Set_sng x1}</span><span class='hs-varid'>singleton</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a>
<span class=hs-linenum>256: </span><span class='hs-definition'>free</span> <span class='hs-layout'>(</span><span class='hs-conid'>Plus</span> <span class='hs-varid'>e1</span> <span class='hs-varid'>e2</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr
-&gt; {VV : (Data.Set.Base.Set [GHC.Types.Char]) | VV == free x1}</span><span class='hs-varid'>free</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e1}</span><span class='hs-varid'>e1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set [GHC.Types.Char])
-&gt; x2:(Data.Set.Base.Set [GHC.Types.Char])
-&gt; {v : (Data.Set.Base.Set [GHC.Types.Char]) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr
-&gt; {VV : (Data.Set.Base.Set [GHC.Types.Char]) | VV == free x1}</span><span class='hs-varid'>free</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e2}</span><span class='hs-varid'>e2</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>257: </span><span class='hs-definition'>free</span> <span class='hs-layout'>(</span><span class='hs-conid'>Let</span> <span class='hs-varid'>x</span> <span class='hs-varid'>e1</span> <span class='hs-varid'>e2</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr
-&gt; {VV : (Data.Set.Base.Set [GHC.Types.Char]) | VV == free x1}</span><span class='hs-varid'>free</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e1}</span><span class='hs-varid'>e1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set [GHC.Types.Char])
-&gt; x2:(Data.Set.Base.Set [GHC.Types.Char])
-&gt; {v : (Data.Set.Base.Set [GHC.Types.Char]) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>(Data.Set.Base.Set [GHC.Types.Char])</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr
-&gt; {VV : (Data.Set.Base.Set [GHC.Types.Char]) | VV == free x1}</span><span class='hs-varid'>free</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e2}</span><span class='hs-varid'>e2</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set [GHC.Types.Char])
-&gt; x2:(Data.Set.Base.Set [GHC.Types.Char])
-&gt; {v : (Data.Set.Base.Set [GHC.Types.Char]) | v == Set_dif x1 v}</span><span class='hs-varop'>`difference`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:[GHC.Types.Char]
-&gt; {v : (Data.Set.Base.Set [GHC.Types.Char]) | v == Set_sng x1}</span><span class='hs-varid'>singleton</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
</pre>

\newthought{An Expression is Closed} with respect to an environment
`G` if all the *free* variables in the expression appear in `G`, i.e.
the environment contains bindings for all the variables in the
expression that are *not* bound within the expression. As we've seen
repeatedly, often a whole pile of informal handwaving, can be
succinctly captured by a type definition that says the `free` variables
in the `Expr` must be contained in the `keys` of the environment `G`:


<pre><span class=hs-linenum>269: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ClosedExpr</span> <span class='hs-conid'>G</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Subset</span> <span class='hs-layout'>(</span><span class='hs-varid'>free</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>keys</span> <span class='hs-conid'>G</span><span class='hs-layout'>)</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\newthought{Closed Evaluation} never goes wrong, i.e. we can
ensure that `eval` will not crash with unbound variables, as
long as it is invoked with suitable environments:


<pre><span class=hs-linenum>277: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>eval</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>g</span><span class='hs-conop'>:</span><span class='hs-conid'>Env</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ClosedExpr</span> <span class='hs-varid'>g</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Val</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent We can be sure an `Expr` is well-scoped if it has *no*
free variables.Lets use that to write a "top-level" evaluator:


<pre><span class=hs-linenum>284: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>topEval</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Expr</span> <span class='hs-keyword'>| Empty (free v)}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Val</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>285: </span><a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | Set_emp free v}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-definition'>topEval</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})
-&gt; {v : AssocativeMap.Expr | Set_sub free v keys x1}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>eval</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | false}) | Set_emp keys v}</span><span class='hs-varid'>emp</span></a> 
</pre>

\exercise Complete the definition of the below function which
*checks* if an `Expr` is well formed before `eval`uating it:


<pre><span class=hs-linenum>292: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>evalAny</span>   <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Env</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-conid'>Val</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>293: </span><a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})
-&gt; AssocativeMap.Expr
-&gt; (Data.Maybe.Maybe {v : AssocativeMap.Expr | val v})</span><span class='hs-definition'>evalAny</span></a> <a class=annot href="#"><span class=annottext>(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>AssocativeMap.Expr</span><span class='hs-varid'>e</span></a>
<span class=hs-linenum>294: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | false}</span><span class='hs-varid'>ok</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : AssocativeMap.Expr | false}
-&gt; {v : (Data.Maybe.Maybe {v : AssocativeMap.Expr | false}) | isJust v &lt;=&gt; true &amp;&amp; fromJust v == x1}</span><span class='hs-conid'>Just</span></a> <a class=annot href="#"><span class=annottext>({v : AssocativeMap.Expr | false}
 -&gt; {v : (Data.Maybe.Maybe {v : AssocativeMap.Expr | false}) | false})
-&gt; {v : AssocativeMap.Expr | false}
-&gt; {v : (Data.Maybe.Maybe {v : AssocativeMap.Expr | false}) | false}</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v})
-&gt; {v : AssocativeMap.Expr | Set_sub free v keys x1}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>eval</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map [GHC.Types.Char] {v : AssocativeMap.Expr | val v}) | v == g}</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | v == e}</span><span class='hs-varid'>e</span></a>
<span class=hs-linenum>295: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (Data.Maybe.Maybe {v : AssocativeMap.Expr | false}) | isJust v &lt;=&gt; false}</span><span class='hs-conid'>Nothing</span></a>
<span class=hs-linenum>296: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>297: </span>    <a class=annot href="#"><span class=annottext>forall a. {VV : a | false}</span><span class='hs-varid'>ok</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | false}</span><span class='hs-varid'>undefined</span></a>  
</pre>

\noindent Proof is all well and good, in the end, you need a few
sanity tests to kick the tires. So:


<pre><span class=hs-linenum>304: </span><a class=annot href="#"><span class=annottext>[AssocativeMap.Expr]</span><span class='hs-definition'>tests</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [AssocativeMap.Expr] | null v &lt;=&gt; false &amp;&amp; xListSelector v == v2 &amp;&amp; len v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &amp;&amp; v == v1}</span><span class='hs-varid'>v1</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &amp;&amp; v == v2}</span><span class='hs-varid'>v2</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>305: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>306: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>v1</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | Set_emp free v}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>topEval</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; v == e1}</span><span class='hs-varid'>e1</span></a></span>          <span class='hs-comment'>-- Rejected by LH</span>
<span class=hs-linenum>307: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>v2</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | Set_emp free v}
-&gt; {v : AssocativeMap.Expr | val v}</span><span class='hs-varid'>topEval</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; v == e2 &amp;&amp; free v == Set_cup free c10 Set_dif free e1 Set_sng x}</span><span class='hs-varid'>e2</span></a>          <span class='hs-comment'>-- Accepted by LH</span>
<span class=hs-linenum>308: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; false}</span><span class='hs-varid'>e1</span></a>  <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:[GHC.Types.Char]
-&gt; {v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; free v == Set_sng x1}</span><span class='hs-conid'>Var</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | null v &lt;=&gt; false &amp;&amp; v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:AssocativeMap.Expr
-&gt; x2:AssocativeMap.Expr
-&gt; {v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; free v == Set_cup free x1 free x2}</span><span class='hs-varop'>`Plus`</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; v == c1 &amp;&amp; free v == Set_empty 0}</span><span class='hs-varid'>c1</span></a> 
<span class=hs-linenum>309: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; free v == Set_cup free c10 Set_dif free e1 Set_sng x}</span><span class='hs-varid'>e2</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[GHC.Types.Char]
-&gt; x2:AssocativeMap.Expr
-&gt; x3:AssocativeMap.Expr
-&gt; {v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; free v == Set_cup free x2 Set_dif free v Set_sng x1}</span><span class='hs-conid'>Let</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | null v &lt;=&gt; false &amp;&amp; v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; v == c10 &amp;&amp; free v == Set_empty 0}</span><span class='hs-varid'>c10</span></a> <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; false &amp;&amp; v == e1}</span><span class='hs-varid'>e1</span></a> 
<span class=hs-linenum>310: </span>    <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | null v &lt;=&gt; false}</span><span class='hs-varid'>x</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | null v &lt;=&gt; false}</span><span class='hs-str'>"x"</span></a>
<span class=hs-linenum>311: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; free v == Set_empty 0}</span><span class='hs-varid'>c1</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int
-&gt; {v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; free v == Set_empty 0}</span><span class='hs-conid'>Const</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a>
<span class=hs-linenum>312: </span>    <a class=annot href="#"><span class=annottext>{v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; free v == Set_empty 0}</span><span class='hs-varid'>c10</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int
-&gt; {v : AssocativeMap.Expr | val v &lt;=&gt; true &amp;&amp; free v == Set_empty 0}</span><span class='hs-conid'>Const</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (10  :  int)}</span><span class='hs-num'>10</span></a>
</pre>

\exercisen{Functions and Closures} \doublestar Extend the language above
to include functions. That is, extend


<pre><span class=hs-linenum>319: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>=</span> <span class='hs-varop'>...</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Fun</span> <span class='hs-conid'>Var</span> <span class='hs-conid'>Expr</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>App</span> <span class='hs-conid'>Expr</span> <span class='hs-conid'>Expr</span>
</pre>

Just focus on ensuring the safety of variable lookups;
ensuring full type-safety (i.e. every application is to
a function) is rather more complicated and beyond the
scope of what we've seen so far.


Implementing Maps: Binary Search Trees  
--------------------------------------

We just saw how easy it is to *use* the Associative
Map [API](#mapapi) to ensure the safety of lookups,
even though the `Map` has a "dynamically" generated
set of keys. Next, lets see how we can *implement*
a `Map` library that respects the API using
[Binary Search Trees](#binarysearchtree)

\newthought{Data Type} First, lets provide an
implementation of the (hitherto abstract) data
type for `Map`. We shall use Binary Search Trees,
wherein, at each `Node`, the `left` (resp. `right`)
subtree has keys that are less than (resp. greater than)
the root `key`.


<pre><span class=hs-linenum>346: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Node</span> <span class='hs-layout'>{</span> <span class='hs-varid'>key</span>   <span class='hs-keyglyph'>::</span> <span class='hs-varid'>k</span>
<span class=hs-linenum>347: </span>                        <span class='hs-layout'>,</span> <span class='hs-varid'>value</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>348: </span>                        <span class='hs-layout'>,</span> <span class='hs-varid'>left</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Map</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>key</span><span class='hs-layout'>}</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>349: </span>                        <span class='hs-layout'>,</span> <span class='hs-varid'>right</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Map</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>key</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-varid'>v</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>350: </span>                 <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Tip</span> 
<span class=hs-linenum>351: </span>  <span class='hs-keyword'>@-}</span>
</pre>

\noindent [Recall](#binarysearchtree) that the above
refined data definition yields strengthened data
constructors that statically ensure that only legal,
*binary-search ordered* trees are created in the program.

\newthought{Defined Keys} Next, we must provide an
implementation of the notion of the `keys` that are
defined for a given `Map`.  This is achieved via the
(lifted) measure function:


<pre><span class=hs-linenum>365: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>keys</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>366: </span><span class='hs-definition'>keys</span>                <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Set</span> <span class='hs-varid'>k</span>
<span class=hs-linenum>367: </span><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:(AssocativeMap.Map a b)
-&gt; {VV : (Data.Set.Base.Set a) | VV == keys x2}</span><span class='hs-definition'>keys</span></a> <span class='hs-conid'>Tip</span>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Data.Set.Base.Set a) | Set_emp v}</span><span class='hs-varid'>empty</span></a>
<span class=hs-linenum>368: </span><span class='hs-definition'>keys</span> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varid'>union</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:a -&gt; {v : (Data.Set.Base.Set a) | v == Set_sng x1}</span><span class='hs-varid'>singleton</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varid'>union</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:(AssocativeMap.Map a b)
-&gt; {VV : (Data.Set.Base.Set a) | VV == keys x2}</span><span class='hs-varid'>keys</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:(AssocativeMap.Map a b)
-&gt; {VV : (Data.Set.Base.Set a) | VV == keys x2}</span><span class='hs-varid'>keys</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
</pre>

Armed with the basic type and measure definition, we
can start to fill in the operations for `Maps`.

\exercisen{Empty Maps} To make sure you are following,
fill in the definition for an `emp`ty Map:


<pre><span class=hs-linenum>378: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>emp</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{m:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>| NoKey m}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>379: </span><a class=annot href="#"><span class=annottext>forall a b. {m : (AssocativeMap.Map a b) | Set_emp keys m}</span><span class='hs-definition'>emp</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>  
</pre>

\exercisen{Insert} To add a key `k'` to a `Map` we
recursively traverse the `Map` zigging `left` or `right`
depending on the result of comparisons with the keys along
the path. Unfortunately, the version below has an
(all too common!) bug, and hence, is *rejected*
by LiquidHaskell. Find and fix the bug so that
the function is verified.


<pre><span class=hs-linenum>391: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>set</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{n:</span> <span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>| PlusKey k m n}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>392: </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; b
-&gt; x4:(AssocativeMap.Map a b)
-&gt; {n : (AssocativeMap.Map a b) | keys n == Set_cup Set_sng x2 keys x4}</span><span class='hs-definition'>set</span></a></span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>v'</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>393: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:b
-&gt; x3:(AssocativeMap.Map {VV : a | VV &lt; x1} b)
-&gt; x4:(AssocativeMap.Map {VV : a | x1 &lt; VV} b)
-&gt; {v : (AssocativeMap.Map a b) | keys v == Set_cup Set_sng x1 Set_cup keys x3 keys x4 &amp;&amp; left v == x3 &amp;&amp; right v == x4 &amp;&amp; value v == x2 &amp;&amp; key v == x1}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == v'}</span><span class='hs-varid'>v'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a>
<span class=hs-linenum>394: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a>  <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; b
-&gt; x4:(AssocativeMap.Map a b)
-&gt; {n : (AssocativeMap.Map a b) | keys n == Set_cup Set_sng x2 keys x4}</span><span class='hs-varid'>set</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == v}</span><span class='hs-varid'>v</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a>
<span class=hs-linenum>395: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; b
-&gt; x4:(AssocativeMap.Map a b)
-&gt; {n : (AssocativeMap.Map a b) | keys n == Set_cup Set_sng x2 keys x4}</span><span class='hs-varid'>set</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == v}</span><span class='hs-varid'>v</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a>
<span class=hs-linenum>396: </span><span class='hs-definition'>set</span> <span class='hs-varid'>k'</span> <span class='hs-varid'>v'</span> <span class='hs-conid'>Tip</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:b
-&gt; x3:(AssocativeMap.Map {VV : a | VV &lt; x1} b)
-&gt; x4:(AssocativeMap.Map {VV : a | x1 &lt; VV} b)
-&gt; {v : (AssocativeMap.Map a b) | keys v == Set_cup Set_sng x1 Set_cup keys x3 keys x4 &amp;&amp; left v == x3 &amp;&amp; right v == x4 &amp;&amp; value v == x2 &amp;&amp; key v == x1}</span><span class='hs-conid'>Node</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == v'}</span><span class='hs-varid'>v'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | false} b) | keys v == Set_empty 0}</span><span class='hs-conid'>Tip</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | false} b) | keys v == Set_empty 0}</span><span class='hs-conid'>Tip</span></a>
</pre>

\newthought{Lookup} Next, lets write the `mem` function that returns
the value associated with a key `k'`. To do so we just compare `k'`
with the root key, if they are equal, we return the binding, and
otherwise we go down the `left` (resp. `right`) subtree if sought for
key is less (resp. greater) than the root `key`. Crucially, we want to
check that lookup *never fails*, and hence, we implement the `Tip`
(i.e. empty) case with `die` gets LiquidHaskell to prove that that
case is indeed dead code, i.e. never happens at run-time.


<pre><span class=hs-linenum>409: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>get'</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span>  <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-keyword'>{Map k v | HasKey k m}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>410: </span><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-definition'>get'</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{m : (AssocativeMap.Map a b) | Set_mem k' keys m}</span><span class='hs-varid'>m</span></a><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>411: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == v}</span><span class='hs-varid'>v</span></a>
<span class=hs-linenum>412: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a>  <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-varid'>get'</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a></span>
<span class=hs-linenum>413: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-varid'>get'</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a></span>
<span class=hs-linenum>414: </span><span class='hs-definition'>get'</span>  <span class='hs-keyword'>_</span> <span class='hs-conid'>Tip</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a>  <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | len v &gt;= 0}</span><span class='hs-str'>"Lookup Never Fails"</span></a>
</pre>

\newthought{Unfortunately} the function above is *rejected*
by LiquidHaskell. This is a puzzler (and a bummer!) because
in fact it *is* correct. So what gives?
Well, lets look at the error for the call `get' k' l`

\begin{liquiderror}
 src/07-case-study-associative-maps.lhs:411:25: Error: Liquid Type Mismatch
   Inferred type
     VV : (Map a b) | VV == l
  
   not a subtype of Required type
     VV : (Map a b) | Set_mem k' (keys VV)

   In Context
     VV : (Map a b) | VV == l
     k  : a
     l  : (Map a b)
     k' : a
\end{liquiderror}

\noindent LiquidHaskell is *unable* to deduce that the the key `k'`
definitely belongs in the `left` subtree `l`. Well, lets ask ourselves:
*why* must `k'` belong in the left subtree? From the input, we know
`HasKey k' m` i.e. that `k'` is *somewhere* in `m`.
That is *one of* the following holds:

1. `k' == k` or,
2. `HasKey k' l` or,
3. `HasKey k' r`.

As the preceding guard `k' == k` fails, we (and LiquidHaskell)
can rule out case (1). Now, what about the `Map` tells us that
case (2) must hold, i.e. that case (3) cannot hold? The *BST invariant*,
all keys in `r` exceed `k` which itself exceeds `k'`. That is,
all nodes in `r` are *disequal* to `k'` and hence `k'` cannot
be in `r`, ruling out case (3). Formally, we need the fact that:
$$\forall\ \vkey,\ \vt. \vt :: {\vMap\ \reft{\vkey'}{k}{\vkey' \not = \vkey}\ v}
                        \ \Rightarrow\
                        \lnot (\vHasKey\ \vkey\ \vt)$$

\newthought{Conversion Lemmas} Unfortunately, LiquidHaskell
*cannot automatically* deduce facts like the above, as they
relate refinements of a container's *type parameters*
(here: $\vkey' \not = \vkey$, which refines the `Map`s first type
parameter) with properties of the entire container
(here: $\vHasKey\ \vkey\ \vt$).
\footnotetext{Why not? This is tricky to describe. Intuitively,
because there is no way of automatically connecting the *traversal*
corresponding to `keys` with the type variable `k`. I wish I had a
better way to explain this rather subtle point; suggestions welcome!}
Fortunately, it is both easy to *state*, *prove* and *use* facts like
the above.

\newthought{Defining Lemmas} To state a lemma, we need only
convert it into a [type](curry-howard) by viewing universal
quantifiers as function parameters, and implications as
function types:


<pre><span class=hs-linenum>476: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>lemma_notMem</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>key</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-keyword'>{k:</span><span class='hs-definition'>k</span> <span class='hs-keyword'>| k /= key}</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| not (HasKey key m)}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>477: </span><a class=annot href="#"><span class=annottext>forall a b.
x1:a
-&gt; x2:(AssocativeMap.Map {VV : a | VV /= x1} b)
-&gt; {v : GHC.Types.Bool | not (Set_mem x1 keys x2)}</span><span class='hs-definition'>lemma_notMem</span></a> <span class='hs-keyword'>_</span>   <span class='hs-conid'>Tip</span>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a> 
<span class=hs-linenum>478: </span><span class='hs-definition'>lemma_notMem</span> <span class='hs-varid'>key</span> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
x1:a
-&gt; x2:(AssocativeMap.Map {VV : a | VV /= x1} b)
-&gt; {v : GHC.Types.Bool | not (Set_mem x1 keys x2)}</span><span class='hs-varid'>lemma_notMem</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>key</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map a b) | v == l}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Bool
-&gt; x2:GHC.Types.Bool
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Prop x1 &amp;&amp; Prop v}</span><span class='hs-varop'>&amp;&amp;</span></a> <a class=annot href="#"><span class=annottext>forall a b.
x1:a
-&gt; x2:(AssocativeMap.Map {VV : a | VV /= x1} b)
-&gt; {v : GHC.Types.Bool | not (Set_mem x1 keys x2)}</span><span class='hs-varid'>lemma_notMem</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>key</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map a b) | v == r}</span><span class='hs-varid'>r</span></a> 
</pre>

\newthought{Proving Lemmas} Note how the signature for `lemma_notMem`
corresponds exactly to the missing fact from above. The "output" type
is a `Bool` refined with the proposition that we desire. We *prove*
the lemma simply by *traversing* the tree which lets LiquidHaskell
build up a proof for the output fact by inductively combining the
proofs from the subtrees.

\newthought{Using Lemmas} To use a lemma, we need to *instantiate*
it to the particular keys and trees we care about, by "calling" the
lemma function, and forcing its result to be in the *environment* used
to typecheck the expression where we want to use the lemma. Say what?
Here is a verified `get`:


<pre><span class=hs-linenum>495: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>get</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-keyword'>{Map k v | HasKey k m}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>496: </span><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-definition'>get</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k'</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>497: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == v}</span><span class='hs-varid'>v</span></a>
<span class=hs-linenum>498: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a>  <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Bool -&gt; a -&gt; a</span><span class='hs-varid'>assert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b.
x1:a
-&gt; x2:(AssocativeMap.Map {VV : a | VV /= x1} b)
-&gt; {v : GHC.Types.Bool | not (Set_mem x1 keys x2)}</span><span class='hs-varid'>lemma_notMem</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(a -&gt; a) -&gt; a -&gt; a</span><span class='hs-varop'>$</span></a>
<span class=hs-linenum>499: </span>                  <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-varid'>get</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a>
<span class=hs-linenum>500: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Bool -&gt; a -&gt; a</span><span class='hs-varid'>assert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b.
x1:a
-&gt; x2:(AssocativeMap.Map {VV : a | VV /= x1} b)
-&gt; {v : GHC.Types.Bool | not (Set_mem x1 keys x2)}</span><span class='hs-varid'>lemma_notMem</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(a -&gt; a) -&gt; a -&gt; a</span><span class='hs-varop'>$</span></a>
<span class=hs-linenum>501: </span>                  <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a -&gt; {m : (AssocativeMap.Map a b) | Set_mem x2 keys m} -&gt; b</span><span class='hs-varid'>get</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a>
<span class=hs-linenum>502: </span><span class='hs-definition'>get</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>Tip</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Types.Char] | len v &gt;= 0}</span><span class='hs-str'>"Lookup failed? Impossible."</span></a>
</pre>

By calling `lemma_notMem` we create a dummy `Bool` that carries the desired
refinement that tells LiquidHaskell that `not (HasKey k' r)` (resp. `not (HasKey k' l)`).
We force the calls to `get k' l` (resp. `get k' r`) to be typechecked using
the materialized refinement by wrapping the calls within a function `assert`


<pre><span class=hs-linenum>511: </span><a class=annot href="#"><span class=annottext>forall a b. a -&gt; b -&gt; b</span><span class='hs-definition'>assert</span></a> <span class='hs-keyword'>_</span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>
</pre>

\newthought{Ghost Values}
This technique of materializing auxiliary facts via *ghost values* is a
well known idea in the program verification literature. Usually, one has
to take care to ensure that ghost computations do not interfere with the
regular computations. If we had to actually *execute*
`lemma_notMem` it would totally wreck the efficient logarithmic lookup
times \footnotetext{Assuming we kept the trees balanced} as we'd traverse
the entire tree all the time
\footnotetext{Which is what makes dynamic contract checking
[rather slow](findler-contract) for such invariants}

\newthought{Laziness} comes to our rescue: as the ghost value is (trivially)
not needed, it is never computed. In fact, it is straightforward to entirely
*erase* the call in the compiled code, which lets us freely `assert` such
`lemma`s to carry out proofs, without paying any runtime penalty. In an eager
language we would have to do a bit of work to specifically mark the computation
as a ghost or [irrelevant](proof-irrelevance) but in the lazy setting we get
this for free.

\exercisen{Membership Test} Capisce? Fix the definition of `mem` so that
it verifiably implements the given signature:


<pre><span class=hs-linenum>537: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>mem</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span><span class='hs-conop'>:</span><span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>m</span><span class='hs-conop'>:</span><span class='hs-conid'>Map</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v &lt;=&gt; HasKey k m}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>538: </span><span class=hs-error><a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; x3:(AssocativeMap.Map a b)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x2 keys x3}</span><span class='hs-definition'>mem</span></a></span> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>k'</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>Node</span> <span class='hs-varid'>k</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>l</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>539: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>
<span class=hs-linenum>540: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>x1:a -&gt; x2:a -&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a>  <a class=annot href="#"><span class=annottext>{VV : a | VV == k}</span><span class='hs-varid'>k</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; x3:(AssocativeMap.Map a b)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x2 keys x3}</span><span class='hs-varid'>mem</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | VV &lt; k} b) | v == l}</span><span class='hs-varid'>l</span></a>
<span class=hs-linenum>541: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a b.
(GHC.Classes.Ord a) =&gt;
x2:a
-&gt; x3:(AssocativeMap.Map a b)
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; Set_mem x2 keys x3}</span><span class='hs-varid'>mem</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == k'}</span><span class='hs-varid'>k'</span></a> <a class=annot href="#"><span class=annottext>{v : (AssocativeMap.Map {VV : a | k &lt; VV} b) | v == r}</span><span class='hs-varid'>r</span></a>
<span class=hs-linenum>542: </span><span class='hs-definition'>mem</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>Tip</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Bool | not (Prop v) &amp;&amp; v == GHC.Types.False}</span><span class='hs-conid'>False</span></a>
</pre>

\exercisen{Fresh} \doublestar To make sure you really understand this business of
ghosts values and proofs, complete the implementation of the following function which
returns a `fresh` integer that is *distinct* from all the values in its input list:


<pre><span class=hs-linenum>550: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>fresh</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Int</span> <span class='hs-keyword'>| not (Elem v xs)}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>551: </span><a class=annot href="#"><span class=annottext>x1:[GHC.Types.Int]
-&gt; {v : GHC.Types.Int | not (Set_mem v elems x1)}</span><span class='hs-definition'>fresh</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. a</span><span class='hs-varid'>undefined</span></a>
</pre>

\noindent To refresh your memory, here are the definitions for `Elem`
we [saw earlier](#listelems)


<pre><span class=hs-linenum>558: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>predicate</span> <span class='hs-conid'>Elem</span> <span class='hs-conid'>X</span> <span class='hs-conid'>Ys</span>  <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Set_mem</span> <span class='hs-conid'>X</span> <span class='hs-layout'>(</span><span class='hs-varid'>elems</span> <span class='hs-conid'>Ys</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>559: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>elems</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>560: </span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-definition'>elems</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a. {v : (Data.Set.Base.Set a) | Set_emp v}</span><span class='hs-varid'>empty</span></a>
<span class=hs-linenum>561: </span><span class='hs-definition'>elems</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:a -&gt; {v : (Data.Set.Base.Set a) | v == Set_sng x1}</span><span class='hs-varid'>singleton</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:(Data.Set.Base.Set a)
-&gt; x2:(Data.Set.Base.Set a)
-&gt; {v : (Data.Set.Base.Set a) | v == Set_cup x1 v}</span><span class='hs-varop'>`union`</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a.
(GHC.Classes.Ord a) =&gt;
x2:[a] -&gt; {VV : (Data.Set.Base.Set a) | VV == elems x2}</span><span class='hs-varid'>elems</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
</pre>

Recap
-----

In this chapter we saw how to combine several of the techniques from previous chapters
in a case study. We learnt how to:

1. **Define** an API for associative maps that used refinements to track the *set* of `keys`
   stored in a map, in order to prevent lookup failures, the `NullPointerDereference` errors
   of the functional world,

2. **Use** the API to implement a small interpreter that is guaranteed to never fail with
   `UnboundVariable` errors, as long as the input expressions were closed,

3. **Implement** the API using Binary Search Trees; in particular, using *ghost lemmas*
   to `assert` facts that LiquidHaskell is otherwise unable to deduce automatically.

