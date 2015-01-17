
Refinement Types
================

\begin{comment}

<pre><span class=hs-linenum>7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>9: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>Intro</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>10: </span>
<span class=hs-linenum>11: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span> <span class='hs-varid'>hiding</span>                   <span class='hs-layout'>(</span><span class='hs-varid'>abs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>12: </span><span class='hs-definition'>divide</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
</pre>
\end{comment}

\newthought{What is a Refinement Type?} In a nutshell, 

$$\mbox{Refinement Types} = \mbox{Types} + \mbox{Logical Predicates}$$

That is, refinement types allow us to decorate types with 
*logical predicates*, which you can think of as *boolean-valued*
Haskell expressions, that constrain the set of values described
by the type. This lets us specify sophisticated invariants of
the underlying values. 

Defining Types
--------------

Let us define some refinement types:


<pre><span class=hs-linenum>32: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Zero</span>    <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>==</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>33: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>NonZero</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>/=</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

The binder `v` is called the *value variable*.
Hence, `Zero` describes the *set of* `Int` values that are equal to `0`,
that is, the singleton set containing just `0`, and `NonZero` describes
the set of `Int` values that are *not* equal to `0`, that is, the set
`1, -1, 2, -2, ...` and so on.
\footnotetext{We will use `@`-marked comments to write refinement type 
annotations the Haskell source file, making these types, quite literally,
machine-checked comments!}

\newthought{To use} these types we can write:


<pre><span class=hs-linenum>48: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zero</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Zero</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>49: </span><a class=annot href="#"><span class=annottext>{v : Int | v == 0}</span><span class='hs-definition'>zero</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>50: </span>
<span class=hs-linenum>51: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>one</span><span class='hs-layout'>,</span> <span class='hs-varid'>two</span><span class='hs-layout'>,</span> <span class='hs-varid'>three</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>NonZero</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>52: </span><a class=annot href="#"><span class=annottext>{v : Int | v /= 0}</span><span class='hs-definition'>one</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>1</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>53: </span><a class=annot href="#"><span class=annottext>{v : Int | v /= 0}</span><span class='hs-definition'>two</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>2</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>54: </span><a class=annot href="#"><span class=annottext>{v : Int | v /= 0}</span><span class='hs-definition'>three</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>3</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
</pre>

Errors
------

If we try to say nonsensical things like:


<pre><span class=hs-linenum>63: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>one'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Zero</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>64: </span><a class=annot href="#"><span class=annottext>{v : Int | v == 0}</span><span class='hs-definition'>one'</span></a> <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>1</span></a></span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
</pre>

\noindent
LH will complain with an error message:

\begin{verbatim}
    02-basic.lhs:58:8: Error: Liquid Type Mismatch
       Inferred type
         VV : Int | VV == (1  :  int)
      
       not a subtype of Required type
         VV : Int | VV == 0
\end{verbatim}

\noindent
The message says that the expression `1 :: Int` has the type

\begin{verbatim}
    {v:Int | v == 1}
\end{verbatim}

\noindent
which is *not* (a subtype of) the *required* type

\begin{verbatim}
    {v:Int | v == 0}
\end{verbatim}

\noindent
as `1` is not equal to `0`.

Subtyping
---------

What is this business of *subtyping*? Suppose we have some more refinements of `Int` 


<pre><span class=hs-linenum>102: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Nat</span>   <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span>        <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>103: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Even</span>  <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varid'>mod</span> <span class='hs-num'>2</span> <span class='hs-varop'>==</span> <span class='hs-num'>0</span> <span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>104: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Lt100</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;</span> <span class='hs-num'>100</span><span class='hs-layout'>}</span>       <span class='hs-keyword'>@-}</span>
</pre>

\newthought{What is the type of} `zero`? `Zero` of course, but also `Nat`:


<pre><span class=hs-linenum>110: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zero'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Nat</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>111: </span><a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-definition'>zero'</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == 0 &amp;&amp; v == Intro.zero}</span><span class='hs-varid'>zero</span></a> 
</pre>

\noindent
and also `Even`:


<pre><span class=hs-linenum>118: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zero''</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Even</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>119: </span><a class=annot href="#"><span class=annottext>{v : Int | v mod 2 == 0}</span><span class='hs-definition'>zero''</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == 0 &amp;&amp; v == Intro.zero}</span><span class='hs-varid'>zero</span></a> 
</pre>

\noindent
and also any other satisfactory refinement, such as:


<pre><span class=hs-linenum>126: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zero'''</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Lt100</span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>127: </span><a class=annot href="#"><span class=annottext>{v : Int | v mod 2 == 0 &amp;&amp; v &lt; 100 &amp;&amp; 0 &lt;= v}</span><span class='hs-definition'>zero'''</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == 0 &amp;&amp; v == Intro.zero}</span><span class='hs-varid'>zero</span></a> 
</pre>

\footnotetext{We use a different names `zero'`, `zero''` etc. as
(currently) LH supports \emph{at most} one refinement type
for each top-level name.}

\newthought{Subtyping and Implication}
`Zero` is the *most precise* type for `0::Int`.
We say most precise because it is *subtype* of `Nat`, `Even` and `Lt100`.
This is because the *set of values* defined by `Zero` is a *subset* of
the values defined by `Nat`, `Even` and `Lt100`, as the following
*logical implications* are valid:

+ $v = 0 \Rightarrow 0 \leq v$
+ $v = 0 \Rightarrow v \ \mbox{mod}\ 2 = 0$
+ $v = 0 \Rightarrow v < 100$

\newthought{Composing Refinements}
In logic, if $P \Rightarrow Q$ and $P \Rightarrow R$ then $P \Rightarrow Q \wedge R$.
Thus, when a term satisfies multiple refinements, we can compose those
refinements with `&&`:


<pre><span class=hs-linenum>151: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zero'''</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>| 0 &lt;= v &amp;&amp; v mod 2 == 0 &amp;&amp; v &lt; 100 }</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>152: </span><span class='hs-definition'>zero''''</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>153: </span><a class=annot href="#"><span class=annottext>Int</span><span class='hs-definition'>zero''''</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
</pre>

\newthought{In Summary} the key points about refinement types are:

1. A refinement type is just a type *decorated* with logical predicates.
2. A term can have *different* refinements for different properties.
3. When we *erase* the predicates we get the standard Haskell types.

\footnotetext{Dually, a standard Haskell type, has the trivial refinement `true`. For example, `Int` is equivalent to `{v:Int | true}`.}

Writing Specifications
----------------------

Lets write some more interesting specifications.

\newthought{Typing Error} We can wrap the usual `error` function:


<pre><span class=hs-linenum>172: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>die</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>String</span> <span class='hs-keyword'>| false}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>173: </span><span class='hs-definition'>die</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>174: </span><a class=annot href="#"><span class=annottext>forall a. {VV : [Char] | false} -&gt; a</span><span class='hs-definition'>die</span></a> <a class=annot href="#"><span class=annottext>{VV : [Char] | false}</span><span class='hs-varid'>msg</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; a</span><span class='hs-varid'>error</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | false}</span><span class='hs-varid'>msg</span></a>
</pre>

The interesting thing about `die` is that the
input type has the refinement `false`, meaning
the function must only be called with `String`s
that satisfy the predicate `false`.

This seems bizarre; isn't it *impossible* to satisfy `false`?
Indeed! Thus, a program containing `die` typechecks
*only* when LH can prove that `die` is *never called*.
For example, LH will *accept*


<pre><span class=hs-linenum>188: </span><a class=annot href="#"><span class=annottext>()</span><span class='hs-definition'>cantDie</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>if</span> <a class=annot href="#"><span class=annottext>Integer</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <span class='hs-num'>1</span> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <span class='hs-num'>3</span>
<span class=hs-linenum>189: </span>            <span class='hs-keyword'>then</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; ()</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"horrible death"</span></a>
<span class=hs-linenum>190: </span>            <span class='hs-keyword'>else</span> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
</pre>

\noindent
by inferring that the branch condition is
always `False` and so `die` cannot be called.
However, LH will *reject* 


<pre><span class=hs-linenum>199: </span><a class=annot href="#"><span class=annottext>()</span><span class='hs-definition'>canDie</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>if</span> <a class=annot href="#"><span class=annottext>Integer</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <span class='hs-num'>1</span> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <span class='hs-num'>2</span>
<span class=hs-linenum>200: </span>           <span class='hs-keyword'>then</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; ()</span><span class='hs-varid'>die</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"horrible death"</span></a></span>
<span class=hs-linenum>201: </span>           <span class='hs-keyword'>else</span> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
</pre>

\noindent
as the branch may (will!) be `True` and so `die` can be called.




Refining Function Types: Preconditions
--------------------------------------

Lets use `die` to write a *safe division* function that
*only accepts* non-zero denominators. 


<pre><span class=hs-linenum>217: </span><span class='hs-definition'>divide'</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>218: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; Int</span><span class='hs-definition'>divide'</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a> <span class='hs-num'>0</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; Int</span><span class='hs-varid'>die</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"divide by zero"</span></a></span>
<span class=hs-linenum>219: </span><span class='hs-definition'>divide'</span> <span class='hs-varid'>n</span> <span class='hs-varid'>d</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == n}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:{v : Int | v /= 0}
-&gt; {v : Int | x1 &gt;= 0 &amp;&amp; x2 &gt;= 0 =&gt; v &gt;= 0 &amp;&amp; x1 &gt;= 0 &amp;&amp; x2 &gt;= 1 =&gt; v &lt;= x1 &amp;&amp; v == x1 / x2}</span><span class='hs-varop'>`div`</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>d</span></a>
</pre>

From the above, it is clear to *us* that `div` is only
called with non-zero divisors. However, LH reports an
error at the call to `"die"` because, what if `divide'`
is actually invoked with a `0` divisor?

We can specify that will not happen, with a *precondition*
that says that the second argument is non-zero:


<pre><span class=hs-linenum>231: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>divide</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>NonZero</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>232: </span><a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v /= 0} -&gt; Int</span><span class='hs-definition'>divide</span></a> <span class='hs-keyword'>_</span> <span class='hs-num'>0</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; Int</span><span class='hs-varid'>die</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"divide by zero"</span></a>
<span class=hs-linenum>233: </span><span class='hs-definition'>divide</span> <span class='hs-varid'>n</span> <span class='hs-varid'>d</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:{v : Int | v /= 0}
-&gt; {v : Int | x1 &gt;= 0 &amp;&amp; x2 &gt;= 0 =&gt; v &gt;= 0 &amp;&amp; x1 &gt;= 0 &amp;&amp; x2 &gt;= 1 =&gt; v &lt;= x1 &amp;&amp; v == x1 / x2}</span><span class='hs-varop'>`div`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v /= 0}</span><span class='hs-varid'>d</span></a>
</pre>

\newthought{To Verify} that `divide` never calls `die`, LH infers
that `"divide by zero"` is not merely of type `String`, but in fact
has the the refined type `{v:String | false}` *in the context* in
which the call to `die'` occurs. LH arrives at this conclusion by
using the fact that in the first equation for `divide` the
*denominator* parameter is in fact

\begin{verbatim}
    0 :: {v: Int | v == 0}
\end{verbatim}

\noindent
which *contradicts* the precondition (i.e. input) type.
Thus, by contradition, LH deduces that the first equation is
*dead code* and hence `die` will not be called at run-time.

\newthought{Establishing Preconditions}
The above signature forces us to ensure that that when we
*use* `divide`, we only supply provably `NonZero` arguments.
Hence, these two uses of `divide` are fine: 


<pre><span class=hs-linenum>258: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; Int</span><span class='hs-definition'>avg2</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>y</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v /= 0} -&gt; Int</span><span class='hs-varid'>divide</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a>
<span class=hs-linenum>259: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; Int -&gt; Int</span><span class='hs-definition'>avg3</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>z</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v /= 0} -&gt; Int</span><span class='hs-varid'>divide</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == z}</span><span class='hs-varid'>z</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a>
</pre>

\exercisen{List Average} Consider the general list-averaging function:


<pre><span class=hs-linenum>265: </span><span class='hs-definition'>avg</span>       <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>266: </span><a class=annot href="#"><span class=annottext>[Int] -&gt; Int</span><span class='hs-definition'>avg</span></a> <a class=annot href="#"><span class=annottext>[Int]</span><span class='hs-varid'>xs</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v /= 0} -&gt; Int</span><span class='hs-varid'>divide</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == total}</span><span class='hs-varid'>total</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v == len xs}</span><span class='hs-varid'>n</span></a></span>
<span class=hs-linenum>267: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>268: </span>    <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>total</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Int] -&gt; Int</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>269: </span>    <a class=annot href="#"><span class=annottext>{v : Int | v == len xs}</span><span class='hs-varid'>n</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Int] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>{v : [Int] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

1. Why does LH flag an error at `n` ?
2. How can you change the code so LH verifies it?

Refining Function Types: Postconditions
---------------------------------------

Next, lets see how we can use refinements to describe the *outputs* of a
function. Consider the following simple *absolute value* function


<pre><span class=hs-linenum>282: </span><span class='hs-definition'>abs</span>           <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>283: </span><a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v &gt;= 0}</span><span class='hs-definition'>abs</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a>
<span class=hs-linenum>284: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt; v}</span><span class='hs-varop'>&lt;</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n}</span><span class='hs-varid'>n</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == n}</span><span class='hs-varid'>n</span></a>
<span class=hs-linenum>285: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n}</span><span class='hs-varid'>n</span></a>
</pre>

We can use a refinement on the output type to specify that the function 
returns non-negative values


<pre><span class=hs-linenum>292: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>abs</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Nat</span> <span class='hs-keyword'>@-}</span>
</pre>

LH *verifies* that `abs` indeed enjoys the above type by
deducing that `n` is trivially non-negative when `0 < n` and that in 
the `otherwise` case, i.e. when `not (0 < n)` the value `0 - n` is
indeed non-negative. \footnotetext{Lets not worry about underflows for the moment.}

\footnotetext{
LH is able to automatically make these arithmetic deductions
by using an [SMT solver](http://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories)
which has decision built-in procedures for arithmetic, to reason about
the logical refinements.}

Testing Values: Booleans and Propositions
-----------------------------------------

In the above example, we *compute* a value that is guaranteed to be a `Nat`.
Sometimes, we need to *test* if a value satisfies some property, e.g., is `NonZero`.
For example, lets write a command-line "calculator" that takes two numbers and divides them.


<pre><span class=hs-linenum>314: </span><a class=annot href="#"><span class=annottext>forall a. (IO a)</span><span class='hs-definition'>calc</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; (IO ())</span><span class='hs-varid'>putStrLn</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Enter numerator"</span></a>
<span class=hs-linenum>315: </span>          <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>(IO Int)</span><span class='hs-varid'>readLn</span></a>
<span class=hs-linenum>316: </span>          <a class=annot href="#"><span class=annottext>[Char] -&gt; (IO ())</span><span class='hs-varid'>putStrLn</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Enter denominator"</span></a>
<span class=hs-linenum>317: </span>          <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>d</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>(IO Int)</span><span class='hs-varid'>readLn</span></a> 
<span class=hs-linenum>318: </span>          <a class=annot href="#"><span class=annottext>[Char] -&gt; (IO ())</span><span class='hs-varid'>putStrLn</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; [Char]</span><span class='hs-varid'>result</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == d}</span><span class='hs-varid'>d</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>319: </span>          <a class=annot href="#"><span class=annottext>(IO a)</span><span class='hs-varid'>calc</span></a> 
</pre>

The function `result` checks if `d` is strictly positive
(and hence, non-zero), and does the division, or otherwise
complains to the user:


<pre><span class=hs-linenum>327: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; [Char]</span><span class='hs-definition'>result</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>d</span></a>
<span class=hs-linenum>328: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>x1:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt; 0}</span><span class='hs-varid'>isPositive</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == d}</span><span class='hs-varid'>d</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"Result = "</span></a> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; x2:[Char] -&gt; {v : [Char] | len v == len x1 + len v}</span><span class='hs-varop'>++</span></a> <a class=annot href="#"><span class=annottext>Int -&gt; [Char]</span><span class='hs-varid'>show</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == n}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v /= 0} -&gt; Int</span><span class='hs-varop'>`divide`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == d}</span><span class='hs-varid'>d</span></a><span class='hs-layout'>)</span> 
<span class=hs-linenum>329: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-str'>"Humph, please enter positive denominator!"</span></a>
</pre>

In the above, `isPositive` is a test that returns a `True` if
its input is strictly greater than `0` or `False` otherwise:


<pre><span class=hs-linenum>336: </span><span class='hs-definition'>isPositive</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>337: </span><a class=annot href="#"><span class=annottext>x1:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt; 0}</span><span class='hs-definition'>isPositive</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
</pre>

\newthought{To verify} the call to `divide` inside `result`
we need to tell LH that the division only happens with a `NonZero`
value `d`. However, the non-zero-ness is established via the *test*
that occurs inside the guard `isPositive d`. Hence, we require a
*post-condition* that states that `isPositive` only returns `True`
when the argument is strictly positive:


<pre><span class=hs-linenum>348: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>isPositive</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v &lt;=&gt; x &gt; 0}</span> <span class='hs-keyword'>@-}</span>
</pre>

In the above signature, read `Prop v` as "`v` is `True`";
dually, read `not (Prop v)` as "`v` is `False`.
Hence, the output type (postcondition) states that
`isPositive x` returns `True` if and only if `x` was in
fact strictly greater than `0`. In other words, we can
write post-conditions for plain-old `Bool`-valued *tests*
to establish that user-supplied values satisfy some desirable
property (here, `Pos` and hence `NonZero`) in order to then
safely perform some computation on it.

\exercise What happens if you *delete* the type for `isPositive` ?
Can you *change* the type for `isPositive` (i.e. write some other type)
to while preserving safety?

\exercisen{Assertions} Consider the following [assert](hoogle-assert) function:


<pre><span class=hs-linenum>368: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>lAssert</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Bool</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>369: </span><a class=annot href="#"><span class=annottext>forall a. Bool -&gt; a -&gt; a</span><span class='hs-definition'>lAssert</span></a> <span class='hs-conid'>True</span>  <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a>
<span class=hs-linenum>370: </span><span class='hs-definition'>lAssert</span> <span class='hs-conid'>False</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | false} -&gt; a</span><span class='hs-varid'>die</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | len v &gt;= 0}</span><span class='hs-str'>"yikes, assertion fails!"</span></a></span>
</pre>

\noindent
We can use the function as:


<pre><span class=hs-linenum>377: </span><span class='hs-definition'>yes</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>378: </span><a class=annot href="#"><span class=annottext>()</span><span class='hs-definition'>yes</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Bool -&gt; () -&gt; ()</span><span class='hs-varid'>lAssert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Integer</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <span class='hs-num'>1</span> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
<span class=hs-linenum>379: </span>
<span class=hs-linenum>380: </span><span class='hs-definition'>no</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>381: </span><a class=annot href="#"><span class=annottext>()</span><span class='hs-definition'>no</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Bool -&gt; () -&gt; ()</span><span class='hs-varid'>lAssert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Integer</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <span class='hs-num'>1</span> <a class=annot href="#"><span class=annottext>x1:Integer -&gt; x2:Integer -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <span class='hs-num'>3</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
</pre>

\noindent
Write a suitable refinement type signature for `lAssert` so that
`lAssert` and `yes` are accepted but `no` is rejected.

\hint You need a precondition that `lAssert` is only called with `True`.

Putting It All Together
-----------------------

Lets wrap up this introduction with a simple `truncate` function 
that connects all the dots. 


<pre><span class=hs-linenum>397: </span><span class='hs-definition'>truncate</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>398: </span><a class=annot href="#"><span class=annottext>Int -&gt; Int -&gt; Int</span><span class='hs-definition'>truncate</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>max</span></a>  
<span class=hs-linenum>399: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Int | v == i' &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>i'</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == max' &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>max'</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == i}</span><span class='hs-varid'>i</span></a>
<span class=hs-linenum>400: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == max' &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>max'</span></a> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:Int
-&gt; {v : Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == i}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v /= 0} -&gt; Int</span><span class='hs-varop'>`divide`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i' &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>i'</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>401: </span>    <span class='hs-keyword'>where</span>
<span class=hs-linenum>402: </span>      <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>i'</span></a>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v &gt;= 0}</span><span class='hs-varid'>abs</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i}</span><span class='hs-varid'>i</span></a>
<span class=hs-linenum>403: </span>      <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>max'</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Int -&gt; {v : Int | v &gt;= 0}</span><span class='hs-varid'>abs</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == max}</span><span class='hs-varid'>max</span></a> 
</pre>

`truncate i n` returns `i` if its absolute value is less the
upper bound `max`, and otherwise *truncates* the value at the maximum.
LH verifies that the use of `divide` is safe by inferring that:

1. `max' < i'` from the branch condition,
2. `0 <= i'`   from the `abs` postcondition, and
3. `0 <= max'` from the `abs` postcondition. 

From the above, LH infers that `i' /= 0`. That is, at the
call site `i' :: NonZero`, thereby satisfying the precondition
for `divide` and verifying that the program has no pesky
divide-by-zero errors.


Recap
-----

This concludes our quick introduction to Refinement Types and
LiquidHaskell. Hopefully you have some sense of how to 

1. **Specify** fine-grained properties of values by decorating their
   types with logical predicates.
2. **Encode** assertions, preconditions, and postconditions with suitable
   function types.
3. **Verify** semantic properties of code by using automatic logic engines 
   (SMT solvers) to track and establish the key relationships between 
   program values.

