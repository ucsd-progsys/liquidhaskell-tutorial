Case Study: Pointers Without Overflows
======================================

\begin{comment}


<pre><span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diffcheck"</span>     <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span><span class='hs-comment'>{-# LANGUAGE ForeignFunctionInterface #-}</span>
<span class=hs-linenum>11: </span>
<span class=hs-linenum>12: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>Memory</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>13: </span>
<span class=hs-linenum>14: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>null</span><span class='hs-layout'>)</span>
<span class=hs-linenum>15: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Char</span>
<span class=hs-linenum>16: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Word</span>
<span class=hs-linenum>17: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>C</span><span class='hs-varop'>.</span><span class='hs-conid'>Types</span>
<span class=hs-linenum>18: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>ForeignPtr</span>
<span class=hs-linenum>19: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>Ptr</span>
<span class=hs-linenum>20: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>Storable</span>
<span class=hs-linenum>21: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>System</span><span class='hs-varop'>.</span><span class='hs-conid'>IO</span><span class='hs-varop'>.</span><span class='hs-conid'>Unsafe</span>
<span class=hs-linenum>22: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>ByteString</span><span class='hs-varop'>.</span><span class='hs-conid'>Internal</span> <span class='hs-layout'>(</span><span class='hs-varid'>c2w</span><span class='hs-layout'>,</span> <span class='hs-varid'>w2c</span><span class='hs-layout'>)</span>
<span class=hs-linenum>23: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Language</span><span class='hs-varop'>.</span><span class='hs-conid'>Haskell</span><span class='hs-varop'>.</span><span class='hs-conid'>Liquid</span><span class='hs-varop'>.</span><span class='hs-conid'>Prelude</span>
</pre>
\end{comment}

A large part of the allure of Haskell is its elegant, high-level ADTs
that ensure that programs won't be plagued by problems like the infamous
[SSL heartbleed bug](heartbleed).
\footnotetext{Assuming, of course, the absence of errors in the compiler and run-time...}
However, another part of Haskell's charm is that when you really really 
need to, you can drop down to low-level pointer twiddling to squeeze the 
most performance out of your machine. But of course, that opens the door 
to the heartbleeds.

Wouldn't it be nice to have have our cake and eat it too? That is wouldn't
it be great if we could twiddle pointers at a low-level and still get the
nice safety assurances of high-level types? In this case study, lets see
how LiquidHaskell lets us do exactly that.


HeartBleeds in Haskell
----------------------

\newthought{Modern Languages} like Haskell are ultimately built upon the
foundation of `C`. Thus, implementation errors could open up unpleasant
vulnerabilities that could easily slither past the type system and even
code inspection.


\newthought{Truncating Strings} As a concrete example, lets look at a
a function that uses the `ByteString` library to truncate strings:


<pre><span class=hs-linenum>55: </span><span class='hs-definition'>chop</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>56: </span><span class='hs-definition'>chop</span> <span class='hs-varid'>s</span> <span class='hs-varid'>n</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>s'</span>
<span class=hs-linenum>57: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>58: </span>    <span class='hs-varid'>b</span>    <span class='hs-keyglyph'>=</span> <span class='hs-conid'>B</span><span class='hs-varop'>.</span><span class='hs-varid'>pack</span> <span class='hs-varid'>s</span>          <span class='hs-comment'>-- down to low-level</span>
<span class=hs-linenum>59: </span>    <span class='hs-varid'>b'</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>B</span><span class='hs-varop'>.</span><span class='hs-varid'>unsafeTake</span> <span class='hs-varid'>n</span> <span class='hs-varid'>b</span>  <span class='hs-comment'>-- grab n chars</span>
<span class=hs-linenum>60: </span>    <span class='hs-varid'>s'</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>B</span><span class='hs-varop'>.</span><span class='hs-varid'>unpack</span> <span class='hs-varid'>b'</span>       <span class='hs-comment'>-- up to high-level</span>
</pre>

\noindent First, the function `pack`s the string into a low-level
bytestring `b`, then it grabs the first `n` `Char`acters from `b`
and translates them back into a high-level `String`. Lets see how
the function works on a small test:


<pre><span class=hs-linenum>69: </span><span class='hs-definition'>ghci</span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>ex</span> <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Ranjit Loves Burritos"</span>
</pre>

\noindent We get the right result when we `chop` a *valid* prefix:


<pre><span class=hs-linenum>75: </span><span class='hs-definition'>ghci</span><span class='hs-varop'>&gt;</span> <span class='hs-varid'>chop</span> <span class='hs-varid'>ex</span> <span class='hs-num'>10</span>
<span class=hs-linenum>76: </span><span class='hs-str'>"Ranjit Lov"</span>
</pre>

\noindent But, as illustrated in Figure~\ref{fig:overflow}, the
machine silently reveals (or more colorfully, *bleeds*) the contents
of adjacent memory or if we use an *invalid* prefix:


<pre><span class=hs-linenum>84: </span><span class='hs-definition'>ghci</span><span class='hs-varop'>&gt;</span> <span class='hs-varid'>heartBleed</span> <span class='hs-varid'>ex</span> <span class='hs-num'>30</span>
<span class=hs-linenum>85: </span><span class='hs-str'>"Ranjit Loves Burritos\NUL\201\&amp;1j\DC3\SOH\NUL"</span>
</pre>


\begin{figure}[h]
\includegraphics[height=1.0in]{img/overflow.png}
\caption{Can we prevent the program from leaking `secret`s?} 
\label{fig:overflow}
\end{figure}


\newthought{Types against Overflows} Now that we have stared the problem
straight in the eye, look at how we can use LiquidHaskell to *prevent* the
above at compile time. To this end, we decompose the overall system into
a hierarchy of *levels* (i.e. *modules*). In this case, we have three levels:

1. **Machine** level `Pointers`
2. **Library** level `ByteString`
3. **User**    level `Application`

\noindent Now, our strategy, as before, is to develop an *refined API* for
each level such that errors at *each* level are prevented by adhering to
the types at *lower* levels.

**HEREHEREHERE**

1. Low-level Pointer API 
------------------------

Strategy: Specify and Verify Types for

<br>

1. **Low-level `Pointer` API**
2. Lib-level `ByteString` API
3. User-level `Application` API

<br>

Errors at *each* level are prevented by types at *lower* levels

\newthought{API: Types}


**Low-level Pointers**


<pre><span class=hs-linenum>132: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span>         
</pre>

<br>

<div class="fragment">
**Foreign Pointers**


<pre><span class=hs-linenum>141: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> 
</pre>

<br>

`ForeignPtr` wraps around `Ptr`; can be exported to/from C.
</div>


\newthought{Operations (1/2)}

<div class="fragment">
**Read** 


<pre><span class=hs-linenum>156: </span><span class='hs-definition'>peek</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>a</span>  
</pre>
</div>

<br>
<div class="fragment">
**Write** 


<pre><span class=hs-linenum>165: </span><span class='hs-definition'>poke</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
</pre>
</div>

<br>

<div class="fragment">
**Arithmetic**

<pre><span class=hs-linenum>174: </span><span class='hs-definition'>plusPtr</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>b</span> 
</pre>
</div>

\newthought{Operations (2/2)}

<div class="fragment">
**Create**


<pre><span class=hs-linenum>184: </span><span class='hs-definition'>malloc</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span>
</pre>
</div>

<br>

<div class="fragment">
**Unwrap and Use**


<pre><span class=hs-linenum>194: </span><span class='hs-definition'>withForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span>     <span class='hs-comment'>-- pointer </span>
<span class=hs-linenum>195: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span>  <span class='hs-comment'>-- action </span>
<span class=hs-linenum>196: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span>             <span class='hs-comment'>-- result</span>
</pre>
</div>

\newthought{Example}

**Allocate a block and write 4 zeros into it**

<div class="fragment">


<pre><span class=hs-linenum>207: </span><a class=annot href="#"><span class=annottext>forall a. (IO (ForeignPtr a))</span><span class='hs-definition'>zero4</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr a) | 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a>
<span class=hs-linenum>208: </span>           <a class=annot href="#"><span class=annottext>x1:(ForeignPtr a)
-&gt; ({v : (Ptr a) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v} -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr a) | fplen fp == plen VV &amp;&amp; zero &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>209: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>210: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>211: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>212: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>213: </span>           <a class=annot href="#"><span class=annottext>(ForeignPtr a) -&gt; (IO (ForeignPtr a))</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a>
<span class=hs-linenum>214: </span>        <span class='hs-keyword'>where</span>
<span class=hs-linenum>215: </span>           <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>zero</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-num'>0</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span>
</pre>

</div>

\newthought{Example}

**Allocate a block and write 4 zeros into it**

How to *prevent overflows* e.g. writing 5 or 50 zeros?

<br>

<div class="fragment">
**Step 1**

*Refine pointers* with allocated size
</div>

<br>

<div class="fragment">
**Step 2**

*Track sizes* in pointer operations
</div>

Refined API: Types

<br>

**1. Refine pointers with allocated size**


<pre><span class=hs-linenum>249: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>plen</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> 
<span class=hs-linenum>250: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>fplen</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> 
</pre>

<br>

<div class="fragment">
Abbreviations for pointers of size `N`


<pre><span class=hs-linenum>259: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>PtrN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span>        <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>|</span>  <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span>  <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> 
<span class=hs-linenum>260: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>|</span>  <span class='hs-varid'>fplen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> 
</pre>
</div>


Refined API: Ops (1/3)

<div class="fragment">
**Create**


<pre><span class=hs-linenum>271: </span><span class='hs-definition'>malloc</span>  <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span>
</pre>
</div> 

<br>

<div class="fragment">
**Unwrap and Use**


<pre><span class=hs-linenum>281: </span><span class='hs-definition'>withForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>fp</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> 
<span class=hs-linenum>282: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>PtrN</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>fplen</span> <span class='hs-varid'>fp</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span>  
<span class=hs-linenum>283: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span>             
</pre>
</div>

Refined API: Ops (2/3)

<br>

**Arithmetic**

Refine type to track *remaining* buffer size

<br>

<div class="fragment">

<pre><span class=hs-linenum>299: </span><span class='hs-definition'>plusPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>300: </span>        <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>o</span><span class='hs-conop'>:</span><span class='hs-layout'>{</span><span class='hs-conid'>Nat</span><span class='hs-keyglyph'>|</span><span class='hs-varid'>o</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>p</span><span class='hs-layout'>}</span>   <span class='hs-comment'>-- in bounds</span>
<span class=hs-linenum>301: </span>        <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>PtrN</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>plen</span> <span class='hs-varid'>b</span> <span class='hs-comment'>-</span> <span class='hs-varid'>o</span><span class='hs-layout'>)</span>   <span class='hs-comment'>-- remainder</span>
</pre>

</div>

Refined API: Ops (3/3)

**Read & Write require non-empty remaining buffer**

<br>

<div class="fragment">
**Read** 


<pre><span class=hs-linenum>316: </span><span class='hs-definition'>peek</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>a</span>  
</pre>
</div>

<br>
<div class="fragment">
**Write** 


<pre><span class=hs-linenum>325: </span><span class='hs-definition'>poke</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>  
</pre>
</div>

\newthought{Overflow Prevented}

How to *prevent overflows* e.g. writing 5 or 50 zeros?

<br>

<div class="fragment">


<pre><span class=hs-linenum>338: </span><a class=annot href="#"><span class=annottext>forall a. (IO (ForeignPtr a))</span><span class='hs-definition'>exBad</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr a) | 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a>
<span class=hs-linenum>339: </span>           <a class=annot href="#"><span class=annottext>x1:(ForeignPtr a)
-&gt; ({v : (Ptr a) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v} -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr a) | fplen fp == plen VV &amp;&amp; zero &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>340: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>341: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>342: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>343: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a></span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>344: </span>           <a class=annot href="#"><span class=annottext>(ForeignPtr a) -&gt; (IO (ForeignPtr a))</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a>
<span class=hs-linenum>345: </span>        <span class='hs-keyword'>where</span>
<span class=hs-linenum>346: </span>           <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>zero</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-num'>0</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span>
</pre>

</div>

2. ByteString API
-----------------

<br>

Strategy: Specify and Verify Types for

<br>

1. Low-level `Pointer` API
2. **Lib-level `ByteString` API**
3. User-level `Application` API

<br>

Errors at *each* level are prevented by types at *lower* levels

\newthought{Type}


<img src="../img/bytestring.png" height=150px>


<pre><span class=hs-linenum>374: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>PS</span> <span class='hs-layout'>{</span>
<span class=hs-linenum>375: </span>    <a class=annot href="#"><span class=annottext>ByteString -&gt; (ForeignPtr Word8)</span><span class='hs-varid'>bPtr</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>376: </span>  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>ByteString -&gt; Int</span><span class='hs-varid'>bOff</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varop'>!</span><span class='hs-conid'>Int</span>
<span class=hs-linenum>377: </span>  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>ByteString -&gt; Int</span><span class='hs-varid'>bLen</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varop'>!</span><span class='hs-conid'>Int</span>
<span class=hs-linenum>378: </span>  <span class='hs-layout'>}</span>
</pre>


\newthought{Refined Type}

<img src="../img/bytestring.png" height=150px>


<pre><span class=hs-linenum>387: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>PS</span> <span class='hs-layout'>{</span>
<span class=hs-linenum>388: </span>      <span class='hs-varid'>bPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>389: </span>    <span class='hs-layout'>,</span> <span class='hs-varid'>bOff</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span><span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span>        <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>bPtr</span><span class='hs-layout'>}</span>
<span class=hs-linenum>390: </span>    <span class='hs-layout'>,</span> <span class='hs-varid'>bLen</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span><span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>+</span> <span class='hs-varid'>bOff</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>bPtr</span><span class='hs-layout'>}</span>
<span class=hs-linenum>391: </span>    <span class='hs-layout'>}</span>                                       <span class='hs-keyword'>@-}</span>
</pre>


<img src="../img/bytestring.png" height=150px>

<br>

**A Useful Abbreviation**


<pre><span class=hs-linenum>402: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span><span class='hs-keyglyph'>|</span> <span class='hs-varid'>bLen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> 
</pre>


<div class="hidden">

<pre><span class=hs-linenum>408: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>bLen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>
</div>

\newthought{Legal Bytestrings}


<br>


<pre><span class=hs-linenum>418: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>good1</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteStringN</span> <span class='hs-num'>5</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>419: </span><a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == 5})</span><span class='hs-definition'>good1</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a>
<span class=hs-linenum>420: </span>           <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == 5 &amp;&amp; v /= Memory.empty}
-&gt; (IO {v : ByteString | bLen v == 5 &amp;&amp; v /= Memory.empty})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>421: </span>
<span class=hs-linenum>422: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>good2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteStringN</span> <span class='hs-num'>3</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>423: </span><a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == 3})</span><span class='hs-definition'>good2</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a>
<span class=hs-linenum>424: </span>           <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == 3 &amp;&amp; v /= Memory.empty}
-&gt; (IO {v : ByteString | bLen v == 3 &amp;&amp; v /= Memory.empty})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a><span class='hs-layout'>)</span>
</pre>

<br>

<div class="fragment">
**Note:** *length* of `good2` is `3` which is *less than* allocated size `5`
</div>

\newthought{Illegal Bytestrings}

<br>


<pre><span class=hs-linenum>438: </span><a class=annot href="#"><span class=annottext>(IO ByteString)</span><span class='hs-definition'>bad1</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> 
<span class=hs-linenum>439: </span>          <a class=annot href="#"><span class=annottext>ByteString -&gt; (IO ByteString)</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (10  :  int)}</span><span class='hs-num'>10</span></a></span><span class='hs-layout'>)</span>
<span class=hs-linenum>440: </span>
<span class=hs-linenum>441: </span><a class=annot href="#"><span class=annottext>(IO ByteString)</span><span class='hs-definition'>bad2</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a>
<span class=hs-linenum>442: </span>          <a class=annot href="#"><span class=annottext>ByteString -&gt; (IO ByteString)</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class='hs-layout'>)</span>
</pre>

<br>

<div class="fragment">
Claimed length *exceeds* allocation ... **rejected** at compile time
</div>

\newthought{`create`}

<div class="hidden">

<pre><span class=hs-linenum>455: </span><span class='hs-definition'>create</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span>
</pre>
</div>

*Allocate* and *fill* a `ByteString`

<br>

<div class="fragment">
**Specification**

<pre><span class=hs-linenum>466: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>create</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>PtrN</span> <span class='hs-conid'>Word8</span> <span class='hs-varid'>n</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span>
<span class=hs-linenum>467: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>n</span>                <span class='hs-keyword'>@-}</span>
</pre>
</div>


<div class="fragment">
**Implementation**


<pre><span class=hs-linenum>476: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({VV : (Ptr Word8) | plen VV == x1 &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>create</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | plen VV == n &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ())</span><span class='hs-varid'>fill</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == n})
-&gt; {v : ByteString | bLen v == n}</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO {v : ByteString | bLen v == n})
 -&gt; {v : ByteString | bLen v == n})
-&gt; (IO {v : ByteString | bLen v == n})
-&gt; {v : ByteString | bLen v == n}</span><span class='hs-varop'>$</span></a> <span class='hs-keyword'>do</span>
<span class=hs-linenum>477: </span>  <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | fplen VV == n &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a>  <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>malloc</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a>
<span class=hs-linenum>478: </span>  <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; fplen v == n &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | plen v == n &amp;&amp; 0 &lt;= plen v} -&gt; (IO ())</span><span class='hs-varid'>fill</span></a> 
<span class=hs-linenum>479: </span>  <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == n}
-&gt; (IO {v : ByteString | bLen v == n})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; fplen v == n &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span>
</pre>
</div>

<!-- CUT
<div class="fragment">
Yikes, there is an error! How to fix?
</div>
-->

\newthought{`pack`}

**Specification**


<pre><span class=hs-linenum>494: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>pack</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>s</span><span class='hs-conop'>:</span><span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>s</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
</pre>

<br>

<div class="fragment">

**Implementation**


<pre><span class=hs-linenum>504: </span><a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : ByteString | bLen v == len x1}</span><span class='hs-definition'>pack</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>str</span></a>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>create</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v == len str}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | plen v == n &amp;&amp; plen v == len xs &amp;&amp; plen v == len str &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}
  -&gt; (IO ()))
 -&gt; {v : ByteString | bLen v == n &amp;&amp; bLen v == len str})
-&gt; ({v : (Ptr Word8) | plen v == n &amp;&amp; plen v == len xs &amp;&amp; plen v == len str &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}
    -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == n &amp;&amp; bLen v == len str}</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | plen v == n &amp;&amp; plen v == len xs &amp;&amp; plen v == len str &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}
-&gt; (IO ())</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | plen VV == n &amp;&amp; plen VV == len xs &amp;&amp; plen VV == len str &amp;&amp; 0 &lt;= plen VV &amp;&amp; n &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | plen v == len str}
-&gt; {v : [Word8] | len v == len str &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; plen v == n &amp;&amp; plen v == len xs &amp;&amp; plen v == len str &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : [Word8] | v == xs &amp;&amp; len v == len str &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>505: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>506: </span>  <a class=annot href="#"><span class=annottext>{v : Int | v == len str}</span><span class='hs-varid'>n</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == str &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>str</span></a>
<span class=hs-linenum>507: </span>  <a class=annot href="#"><span class=annottext>{v : [Word8] | len v == len str}</span><span class='hs-varid'>xs</span></a>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Char -&gt; Word8) -&gt; x3:[Char] -&gt; {v : [Word8] | len v == len x3}</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>Char -&gt; Word8</span><span class='hs-varid'>c2w</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == str &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>str</span></a>
<span class=hs-linenum>508: </span>  <a class=annot href="#"><span class=annottext>forall a.
(Storable [Bivariant]
[] a) =&gt;
x1:{VV : (Ptr a) | plen VV == len str}
-&gt; {VV : [a] | len VV == len str &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>(Ptr a)</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | 0 &lt; plen v} -&gt; a -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(IO ()) -&gt; (IO ()) -&gt; (IO ())</span><span class='hs-varop'>&gt;&gt;</span></a> <a class=annot href="#"><span class=annottext>x1:(Ptr a)
-&gt; {VV : [a] | len VV &gt;= 0 &amp;&amp; len VV &lt;= plen x1 &amp;&amp; len VV &lt;= len str}
-&gt; (IO ())</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr a) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>plusPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>509: </span>  <span class='hs-varid'>go</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>() -&gt; (IO ())</span><span class='hs-varid'>return</span></a>  <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
</pre>

</div>



\newthought{`unsafeTake`}

Extract *prefix* string of size `n`

<br>

<div class="fragment">
**Specification**


<pre><span class=hs-linenum>526: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeTake</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span>
<span class=hs-linenum>527: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-keyword'>{ByteString | n &lt;= bLen b}</span>
<span class=hs-linenum>528: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>n</span>            <span class='hs-keyword'>@-}</span>
</pre>
</div>


<br>

<div class="fragment">
**Implementation**


<pre><span class=hs-linenum>539: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; {b : ByteString | x1 &lt;= bLen b}
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>unsafeTake</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a>
</pre>
</div>

\newthought{`unpack` }

**Specification**


<pre><span class=hs-linenum>548: </span><span class='hs-definition'>unpack</span> 
<span class=hs-linenum>549: </span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>StringN</span> <span class='hs-layout'>(</span><span class='hs-varid'>bLen</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span>
</pre>

<br>

<div class="fragment">
**Implementation**


<pre><span class=hs-linenum>558: </span><span class='hs-definition'>unpack</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>you</span> <span class='hs-varop'>.</span> <span class='hs-varid'>get</span> <span class='hs-varop'>.</span> <span class='hs-varid'>the</span> <span class='hs-varop'>.</span> <span class='hs-varid'>idea</span> <span class='hs-comment'>-- see source </span>
</pre>
</div>

<div class="hidden">

<pre><span class=hs-linenum>564: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>Unpack</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>acc</span><span class='hs-conop'>:</span><span class='hs-varid'>b</span><span class='hs-layout'>,</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-varid'>int</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>len</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>1</span> <span class='hs-varop'>+</span> <span class='hs-varid'>n</span> <span class='hs-varop'>+</span> <span class='hs-varid'>len</span> <span class='hs-varid'>acc</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>565: </span>
<span class=hs-linenum>566: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unpack</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>StringN</span> <span class='hs-layout'>(</span><span class='hs-varid'>bLen</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>567: </span><span class='hs-definition'>unpack</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> 
<span class=hs-linenum>568: </span><a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {VV : [Char] | len VV == bLen x1}</span><span class='hs-definition'>unpack</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-keyword'>_</span>  <span class='hs-keyword'>_</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | null v &lt;=&gt; true &amp;&amp; len v == 0 &amp;&amp; bLens v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>569: </span><span class='hs-definition'>unpack</span> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>ps</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO {v : [Char] | len v &gt; 0}) -&gt; {v : [Char] | len v &gt; 0}</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO {v : [Char] | len v &gt; 0}) -&gt; {v : [Char] | len v &gt; 0})
-&gt; (IO {v : [Char] | len v &gt; 0}) -&gt; {v : [Char] | len v &gt; 0}</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO {v : [Char] | len v &gt; 0}))
-&gt; (IO {v : [Char] | len v &gt; 0})</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>(ForeignPtr Word8)</span><span class='hs-varid'>ps</span></a> <a class=annot href="#"><span class=annottext>((x5:{v : (Ptr Word8) | 0 &lt;= plen v}
  -&gt; (IO {v : [Char] | len v &gt; 0 &amp;&amp; len v &lt;= plen x5}))
 -&gt; (IO {v : [Char] | len v &gt; 0}))
-&gt; (x5:{v : (Ptr Word8) | 0 &lt;= plen v}
    -&gt; (IO {v : [Char] | len v &gt; 0 &amp;&amp; len v &lt;= plen x5}))
-&gt; (IO {v : [Char] | len v &gt; 0})</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>570: </span>   <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen x1}
-&gt; x3:{v : [Char] | len v &gt;= 0}
-&gt; (IO {v : [Char] | len v == 1 + x2 + len x3 &amp;&amp; v /= x3 &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len x3})</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span>  <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; true &amp;&amp; len v == 0 &amp;&amp; bLens v == 0 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>571: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>572: </span>   <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen x1}
-&gt; x3:{v : [Char] | len v &gt;= 0}
-&gt; (IO {v : [Char] | len v == 1 + x2 + len x3 &amp;&amp; v /= x3 &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len x3})</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-num'>0</span> <a class=annot href="#"><span class=annottext>{VV : [Char] | len VV &gt;= 0}</span><span class='hs-varid'>acc</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt; plen v}
-&gt; (IO {v : Word8 | v == deref x1})</span><span class='hs-varid'>peek</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>(IO Word8)
-&gt; (Word8
    -&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc}))
-&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc})</span><span class='hs-varop'>&gt;&gt;=</span></a> <a class=annot href="#"><span class=annottext>Word8
-&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc})</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>e</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>{v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc}
-&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Word8 -&gt; Char</span><span class='hs-varid'>w2c</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == e}</span><span class='hs-varid'>e</span></a> <a class=annot href="#"><span class=annottext>x1:Char
-&gt; x2:[Char]
-&gt; {v : [Char] | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; bLens v == bLen x1 + bLens x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == acc &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>acc</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>573: </span>   <span class='hs-varid'>go</span> <span class='hs-varid'>p</span> <span class='hs-varid'>n</span> <span class='hs-varid'>acc</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt; plen v}
-&gt; (IO {v : Word8 | v == deref x1})</span><span class='hs-varid'>peek</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen p}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(IO Word8)
-&gt; (Word8
    -&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc}))
-&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc})</span><span class='hs-varop'>&gt;&gt;=</span></a>   <a class=annot href="#"><span class=annottext>Word8
-&gt; (IO {v : [Char] | v /= acc &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len acc})</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>e</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; x2:{VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= plen x1}
-&gt; x3:{VV : [Char] | len VV &gt;= 0}
-&gt; (IO {VV : [Char] | len VV == 1 + x2 + len x3 &amp;&amp; VV /= x3 &amp;&amp; len VV &gt; 0 &amp;&amp; len VV &gt; len x3})</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen p}</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Word8 -&gt; Char</span><span class='hs-varid'>w2c</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == e}</span><span class='hs-varid'>e</span></a> <a class=annot href="#"><span class=annottext>x1:Char
-&gt; x2:[Char]
-&gt; {v : [Char] | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; bLens v == bLen x1 + bLens x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == acc &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>acc</span></a><span class='hs-layout'>)</span>
</pre>

</div>

3. Application API 
-------------------

<br>

Strategy: Specify and Verify Types for

<br>

1. Low-level `Pointer` API
2. Lib-level `ByteString` API
3. **User-level `Application` API**

<br>

Errors at *each* level are prevented by types at *lower* levels

\newthought{HeartBleed Revisited}

Lets revisit our potentially "bleeding" `chop`

<br>

<div class="hidden">

<pre><span class=hs-linenum>603: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>StringN</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>String</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>len</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>
</div>
<div class="fragment">


<pre><span class=hs-linenum>609: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>chop</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>s</span><span class='hs-conop'>:</span><span class='hs-conid'>String</span>
<span class=hs-linenum>610: </span>         <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-keyword'>{Nat | n &lt;= len s}</span>
<span class=hs-linenum>611: </span>         <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>StringN</span> <span class='hs-varid'>n</span>
<span class=hs-linenum>612: </span>  <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>613: </span><a class=annot href="#"><span class=annottext>x1:[Char]
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1}
-&gt; {VV : [Char] | len VV == x2}</span><span class='hs-definition'>chop</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0 &amp;&amp; v &lt;= len s}</span><span class='hs-varid'>n</span></a> <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>{v : [Char] | v == s' &amp;&amp; len v == bLen b' &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>s'</span></a>
<span class=hs-linenum>614: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>615: </span>    <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == len s}</span><span class='hs-varid'>b</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : ByteString | bLen v == len x1}</span><span class='hs-varid'>pack</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == s &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>s</span></a>          <span class='hs-comment'>-- down to low-level</span>
<span class=hs-linenum>616: </span>    <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == n}</span><span class='hs-varid'>b'</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; {v : ByteString | x1 &lt;= bLen v}
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>unsafeTake</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= len s}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == b &amp;&amp; bLen v == len s &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>b</span></a>  <span class='hs-comment'>-- grab n chars</span>
<span class=hs-linenum>617: </span>    <a class=annot href="#"><span class=annottext>{v : [Char] | len v == bLen b'}</span><span class='hs-varid'>s'</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : [Char] | len v == bLen x1}</span><span class='hs-varid'>unpack</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == b' &amp;&amp; bLen v == n &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>b'</span></a>       <span class='hs-comment'>-- up to high-level</span>
</pre>

<!-- BEGIN CUT 
<br>

Yikes! How shall we fix it?

     END CUT -->
</div>

<!-- BEGIN CUT

A Well Typed `chop`
-------------------


<pre><span class=hs-linenum>634: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>chop</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>s</span><span class='hs-conop'>:</span><span class='hs-conid'>String</span>
<span class=hs-linenum>635: </span>         <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-keyword'>{Nat | n &lt;= len s}</span>
<span class=hs-linenum>636: </span>         <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>String</span> <span class='hs-keyword'>| len v = n}</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>637: </span><span class='hs-definition'>chop</span> <span class='hs-varid'>s</span> <span class='hs-varid'>n</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>s'</span>
<span class=hs-linenum>638: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>639: </span>    <span class='hs-varid'>b</span>    <span class='hs-keyglyph'>=</span> <span class='hs-varid'>pack</span> <span class='hs-varid'>s</span>          <span class='hs-comment'>-- down to low-level</span>
<span class=hs-linenum>640: </span>    <span class='hs-varid'>b'</span>   <span class='hs-keyglyph'>=</span> <span class='hs-varid'>unsafeTake</span> <span class='hs-varid'>n</span> <span class='hs-varid'>b</span>  <span class='hs-comment'>-- grab n chars</span>
<span class=hs-linenum>641: </span>    <span class='hs-varid'>s'</span>   <span class='hs-keyglyph'>=</span> <span class='hs-varid'>unpack</span> <span class='hs-varid'>b'</span>       <span class='hs-comment'>-- up to high-level</span>
</pre>

END CUT -->

\newthought{"HeartBleed" no more}

<br>


<pre><span class=hs-linenum>651: </span><a class=annot href="#"><span class=annottext>[[Char]]</span><span class='hs-definition'>demo</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [[Char]] | null v &lt;=&gt; false &amp;&amp; xListSelector v == ex30 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{v : [Char] | v == ex6 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex6</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [Char] | v == ex30 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex30</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>652: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>653: </span>    <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false}</span><span class='hs-varid'>ex</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'N'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'o'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'r'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'m'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'a'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'n'</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>654: </span>    <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>ex6</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char]
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1}
-&gt; {v : [Char] | len v == v}</span><span class='hs-varid'>chop</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false &amp;&amp; v == ex &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (6  :  int)}</span><span class='hs-num'>6</span></a>  <span class='hs-comment'>-- ok</span>
<span class=hs-linenum>655: </span>    <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>ex30</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char]
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1}
-&gt; {v : [Char] | len v == v}</span><span class='hs-varid'>chop</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false &amp;&amp; v == ex &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (30  :  int)}</span><span class='hs-num'>30</span></a></span>  <span class='hs-comment'>-- out of bounds</span>
</pre>

<br>

"Bleeding" `chop ex 30` *rejected* by compiler

Nested ByteStrings 
------------------

For a more in depth example, let's take a look at `group`,
which transforms strings like

   `"foobaaar"`

into *lists* of strings like

   `["f","oo", "b", "aaa", "r"]`.

The specification is that `group` should produce a list of `ByteStrings`

1. that are all *non-empty* (safety)
2. the sum of whose lengths is equal to the length of the input string (precision)

We use the type alias


<pre><span class=hs-linenum>682: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ByteStringNE</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>bLen</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&gt;</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

to specify (safety) and introduce a new measure


<pre><span class=hs-linenum>688: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>bLens</span>  <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>ByteString</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>689: </span>    <span class='hs-varid'>bLens</span> <span class='hs-layout'>(</span><span class='hs-conid'>[]</span><span class='hs-layout'>)</span>   <span class='hs-keyglyph'>=</span> <span class='hs-num'>0</span>
<span class=hs-linenum>690: </span>    <span class='hs-varid'>bLens</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-varid'>bLen</span> <span class='hs-varid'>x</span> <span class='hs-varop'>+</span> <span class='hs-varid'>bLens</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>691: </span>  <span class='hs-keyword'>@-}</span>
</pre>

to specify (precision). The full type-specification looks like this:


<pre><span class=hs-linenum>697: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>group</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>ByteStringNE</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| bLens v = bLen b}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>698: </span><a class=annot href="#"><span class=annottext>x1:ByteString
-&gt; {v : [{v : ByteString | bLen v &gt; 0}] | bLens v == bLen x1}</span><span class='hs-definition'>group</span></a> <a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>699: </span>    <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Bool | Prop v &lt;=&gt; bLen x1 == 0}</span><span class='hs-varid'>null</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>xs</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | null v &lt;=&gt; true &amp;&amp; len v == 0 &amp;&amp; bLens v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>700: </span>    <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>let</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v &gt; 0} -&gt; Word8</span><span class='hs-varid'>unsafeHead</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>701: </span>                      <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : ByteString | VV == ys &amp;&amp; bLen Memory.empty + bLen VV == bLen ys &amp;&amp; VV /= xs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : ByteString | VV == zs &amp;&amp; bLen Memory.empty + bLen VV == bLen zs &amp;&amp; VV /= xs}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8
-&gt; x2:ByteString
-&gt; (ByteString, ByteString)&lt;\x3 VV -&gt; bLen x3 + bLen v == bLen x2&gt;</span><span class='hs-varid'>spanByte</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : ByteString | bLen v &gt; 0} -&gt; Word8</span><span class='hs-varid'>unsafeHead</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : ByteString | bLen v &gt; 0}
-&gt; {v : ByteString | bLen v == bLen x1 - 1}</span><span class='hs-varid'>unsafeTail</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>702: </span>                  <span class='hs-keyword'>in</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Word8 | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>Word8 -&gt; x2:ByteString -&gt; {v : ByteString | bLen v == 1 + bLen v}</span><span class='hs-varop'>`cons`</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == ys &amp;&amp; v == ys &amp;&amp; bLen Memory.empty + bLen v == bLen ys &amp;&amp; v /= xs &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:{v : ByteString | v /= Memory.empty &amp;&amp; bLen v &gt; 0}
-&gt; x2:[{v : ByteString | v /= Memory.empty &amp;&amp; bLen v &gt; 0}]&lt;\_ VV -&gt; v /= Memory.empty &amp;&amp; bLen v &gt; 0&gt;
-&gt; {v : [{v : ByteString | v /= Memory.empty &amp;&amp; bLen v &gt; 0}]&lt;\_ VV -&gt; v /= Memory.empty &amp;&amp; bLen v &gt; 0&gt; | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2 &amp;&amp; bLens v == bLen x1 + bLens x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>x1:ByteString
-&gt; {v : [{v : ByteString | bLen v &gt; 0}] | bLens v == bLen x1}</span><span class='hs-varid'>group</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == zs &amp;&amp; v == zs &amp;&amp; bLen Memory.empty + bLen v == bLen zs &amp;&amp; v /= xs &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>zs</span></a>
</pre>

As you can probably tell, `spanByte` appears to be doing a lot of the work here,
so let's take a closer look at it to see why the post-condition holds.


<pre><span class=hs-linenum>709: </span><span class='hs-definition'>spanByte</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteString</span><span class='hs-layout'>,</span> <span class='hs-conid'>ByteString</span><span class='hs-layout'>)</span>
<span class=hs-linenum>710: </span><a class=annot href="#"><span class=annottext>Word8
-&gt; x2:ByteString
-&gt; (ByteString, ByteString)&lt;\x14 VV -&gt; bLen x14 + bLen VV == bLen x2&gt;</span><span class='hs-definition'>spanByte</span></a> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-varid'>ps</span></a><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO (ByteString, ByteString)&lt;\x10 VV -&gt; bLen x10 + bLen v == bLen ps&gt;)
-&gt; (ByteString, ByteString)&lt;\x3 VV -&gt; bLen x3 + bLen v == bLen ps&gt;</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO (ByteString, ByteString)&lt;\x23 VV -&gt; bLen x23 + bLen v == bLen ps&gt;)
 -&gt; (ByteString, ByteString)&lt;\x16 VV -&gt; bLen x16 + bLen v == bLen ps&gt;)
-&gt; (IO (ByteString, ByteString)&lt;\x23 VV -&gt; bLen x23 + bLen v == bLen ps&gt;)
-&gt; (ByteString, ByteString)&lt;\x16 VV -&gt; bLen x16 + bLen v == bLen ps&gt;</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO (ByteString, ByteString)&lt;\x11 VV -&gt; bLen x11 + bLen v == bLen ps&gt;))
-&gt; (IO (ByteString, ByteString)&lt;\x11 VV -&gt; bLen x11 + bLen v == bLen ps&gt;)</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
  -&gt; (IO (ByteString, ByteString)&lt;\x33 VV -&gt; bLen x33 + bLen v == bLen ps&gt;))
 -&gt; (IO (ByteString, ByteString)&lt;\x25 VV -&gt; bLen x25 + bLen v == bLen ps&gt;))
-&gt; ({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
    -&gt; (IO (ByteString, ByteString)&lt;\x33 VV -&gt; bLen x33 + bLen v == bLen ps&gt;))
-&gt; (IO (ByteString, ByteString)&lt;\x25 VV -&gt; bLen x25 + bLen v == bLen ps&gt;)</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | fplen x == plen VV &amp;&amp; 0 &lt;= plen VV &amp;&amp; l &lt;= plen VV &amp;&amp; s &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>711: </span>    <a class=annot href="#"><span class=annottext>{v : (Ptr (Any *)) | l &lt;= plen v}
-&gt; x2:{v : Int | v == 0 &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l &amp;&amp; v &lt;= s}
-&gt; (IO (ByteString, ByteString)&lt;\x4 VV -&gt; bLen x4 + bLen v == bLen ps &amp;&amp; x2 &lt;= bLen v&gt;)</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr (Any *)) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>712: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>713: </span>    <a class=annot href="#"><span class=annottext>forall a.
{VV : (Ptr a) | l &lt;= plen VV}
-&gt; x2:{VV : Int | VV == 0 &amp;&amp; VV &gt;= 0 &amp;&amp; VV &lt;= l &amp;&amp; VV &lt;= s}
-&gt; (IO (ByteString, ByteString)&lt;\x1 VV -&gt; bLen x1 + bLen VV == bLen ps &amp;&amp; x2 &lt;= bLen VV&gt;)</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr a) | l &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= l}</span><span class='hs-varid'>i</span></a> <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt;= v}</span><span class='hs-varop'>&gt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(ByteString, ByteString)&lt;\x42 VV -&gt; v == Memory.empty &amp;&amp; bLen v == 0 &amp;&amp; bLen v == bLen ps - l &amp;&amp; bLen v == bLen ps - i &amp;&amp; bLen v == bLen x42 - l &amp;&amp; bLen v == bLen x42 - i &amp;&amp; bLen Memory.empty + bLen v == bLen Memory.empty &amp;&amp; bLen ps + bLen v == bLen ps &amp;&amp; bLen ps + bLen v == bLen x42 &amp;&amp; bLen x42 + bLen v == bLen ps &amp;&amp; bLen x42 + bLen v == bLen x42&gt;
-&gt; (IO (ByteString, ByteString)&lt;\x42 VV -&gt; v == Memory.empty &amp;&amp; bLen v == 0 &amp;&amp; bLen v == bLen ps - l &amp;&amp; bLen v == bLen ps - i &amp;&amp; bLen v == bLen x42 - l &amp;&amp; bLen v == bLen x42 - i &amp;&amp; bLen Memory.empty + bLen v == bLen Memory.empty &amp;&amp; bLen ps + bLen v == bLen ps &amp;&amp; bLen ps + bLen v == bLen x42 &amp;&amp; bLen x42 + bLen v == bLen ps &amp;&amp; bLen x42 + bLen v == bLen x42&gt;)</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ByteString, {v : ByteString | v == Memory.empty &amp;&amp; bLen v == 0 &amp;&amp; bLen v == bLen ps - l &amp;&amp; bLen v == bLen ps - i &amp;&amp; bLen Memory.empty + bLen v == bLen Memory.empty &amp;&amp; bLen ps + bLen v == bLen ps})&lt;\x21 VV -&gt; v == Memory.empty &amp;&amp; bLen v == 0 &amp;&amp; bLen v == bLen ps - l &amp;&amp; bLen v == bLen ps - i &amp;&amp; bLen v == bLen x21 - l &amp;&amp; bLen v == bLen x21 - i &amp;&amp; bLen Memory.empty + bLen v == bLen Memory.empty &amp;&amp; bLen ps + bLen v == bLen ps &amp;&amp; bLen ps + bLen v == bLen x21 &amp;&amp; bLen x21 + bLen v == bLen ps &amp;&amp; bLen x21 + bLen v == bLen x21&gt; | x_Tuple22 v == Memory.empty &amp;&amp; snd v == Memory.empty}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : ByteString | v == ps &amp;&amp; v == Memory.PS x s l &amp;&amp; bLen v == l &amp;&amp; bOff v == s &amp;&amp; bPtr v == x &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>ps</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : ByteString | v == Memory.empty &amp;&amp; bLen v == 0 &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>empty</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>714: </span>           <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>c'</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>p:(Ptr a) -&gt; {v : Int | v &lt; plen p &amp;&amp; 0 &lt;= v} -&gt; (IO Word8)</span><span class='hs-varid'>peekByteOff</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a>
<span class=hs-linenum>715: </span>                            <span class='hs-keyword'>if</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == c}</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>x1:Word8 -&gt; x2:Word8 -&gt; {v : Bool | Prop v &lt;=&gt; x1 /= v}</span><span class='hs-varop'>/=</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == c'}</span><span class='hs-varid'>c'</span></a>
<span class=hs-linenum>716: </span>                                <span class='hs-keyword'>then</span> <a class=annot href="#"><span class=annottext>(ByteString, ByteString)&lt;\x15 VV -&gt; bLen v == bLen ps - i &amp;&amp; bLen x15 + bLen v == bLen ps &amp;&amp; v /= Memory.empty &amp;&amp; bLen v &gt; 0&gt;
-&gt; (IO (ByteString, ByteString)&lt;\x15 VV -&gt; bLen v == bLen ps - i &amp;&amp; bLen x15 + bLen v == bLen ps &amp;&amp; v /= Memory.empty &amp;&amp; bLen v &gt; 0&gt;)</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>(ByteString, {v : ByteString | bLen v == bLen ps - i &amp;&amp; v /= Memory.empty &amp;&amp; bLen v &gt; 0})&lt;\x4 VV -&gt; bLen v == bLen ps - i &amp;&amp; bLen x4 + bLen v == bLen ps &amp;&amp; v /= Memory.empty &amp;&amp; bLen v &gt; 0&gt;</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; {v : ByteString | x1 &lt;= bLen v}
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>unsafeTake</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == ps &amp;&amp; v == Memory.PS x s l &amp;&amp; bLen v == l &amp;&amp; bOff v == s &amp;&amp; bPtr v == x &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>ps</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : ByteString | x1 &lt;= bLen v}
-&gt; {v : ByteString | bLen v == bLen v - x1}</span><span class='hs-varid'>unsafeDrop</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == ps &amp;&amp; v == Memory.PS x s l &amp;&amp; bLen v == l &amp;&amp; bOff v == s &amp;&amp; bPtr v == x &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>ps</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>717: </span>                                <span class='hs-keyword'>else</span> <a class=annot href="#"><span class=annottext>{VV : (Ptr a) | l &lt;= plen VV}
-&gt; {VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= l}
-&gt; (IO (ByteString, ByteString)&lt;\x1 VV -&gt; bLen x1 + bLen VV == bLen ps&gt;)</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span>
</pre>

LiquidHaskell infers that `0 <= i <= l` and therefore that all of the memory
accesses are safe. Furthermore, due to the precise specifications given to
`unsafeTake` and `unsafeDrop`, it is able to prove that `spanByte` has the type


<pre><span class=hs-linenum>725: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>spanByte</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteStringPair</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
</pre>

where `ByteStringPair b` describes a pair of `ByteString`s whose
lengths sum to the length of `b`.


<pre><span class=hs-linenum>732: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ByteStringPair</span> <span class='hs-conid'>B</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteString</span><span class='hs-layout'>,</span> <span class='hs-conid'>ByteString</span><span class='hs-layout'>)</span><span class='hs-varop'>&lt;</span><span class='hs-layout'>{</span><span class='hs-keyglyph'>\</span><span class='hs-varid'>x1</span> <span class='hs-varid'>x2</span> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>733: </span>       <span class='hs-varid'>bLen</span> <span class='hs-varid'>x1</span> <span class='hs-varop'>+</span> <span class='hs-varid'>bLen</span> <span class='hs-varid'>x2</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>bLen</span> <span class='hs-conid'>B</span><span class='hs-layout'>}</span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>@-}</span>
</pre>

Recap: Types Against Overflows
------------------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. Low-level `Pointer` API
2. Lib-level `ByteString` API
3. User-level `Application` API

<br>

**Errors at *each* level are prevented by types at *lower* levels**







\begin{comment}

<pre><span class=hs-linenum>761: </span><span class='hs-comment'>-----------------------------------------------------------------------</span>
<span class=hs-linenum>762: </span><span class='hs-comment'>-- Helper Code</span>
<span class=hs-linenum>763: </span><span class='hs-comment'>-----------------------------------------------------------------------</span>
<span class=hs-linenum>764: </span>
<span class=hs-linenum>765: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeCreate</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>l</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-conid'>PtrN</span> <span class='hs-conid'>Word8</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>766: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({VV : (Ptr Word8) | plen VV == x1 &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>unsafeCreate</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | plen VV == n &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ())</span><span class='hs-varid'>f</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>create</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | plen v == n &amp;&amp; 0 &lt;= plen v} -&gt; (IO ())</span><span class='hs-varid'>f</span></a> <span class='hs-comment'>-- unsafePerformIO $ create n f</span>
<span class=hs-linenum>767: </span>
<span class=hs-linenum>768: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>invariant</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span>   <span class='hs-keyword'>| bLen  v &gt;= 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>769: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>invariant</span> <span class='hs-keyword'>{v:</span><span class='hs-keyglyph'>[</span><span class='hs-conid'>ByteString</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| bLens v &gt;= 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>770: </span>
<span class=hs-linenum>771: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>PLLen</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;=</span> <span class='hs-layout'>(</span><span class='hs-varid'>plen</span> <span class='hs-varid'>p</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>772: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>ForeignPtrN</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-varid'>int</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>n</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>773: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>FPLenPLen</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>fp</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>fp</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>774: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>PtrLen</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>List</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>len</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>775: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>PlenEq</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span> <span class='hs-varid'>int</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>x</span> <span class='hs-varop'>&lt;=</span> <span class='hs-layout'>(</span><span class='hs-varid'>plen</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>776: </span>
<span class=hs-linenum>777: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeHead</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| (bLen v) &gt; 0}</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Word8</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>778: </span>
<span class=hs-linenum>779: </span><span class='hs-definition'>unsafeHead</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>780: </span><a class=annot href="#"><span class=annottext>{v : ByteString | bLen v &gt; 0} -&gt; Word8</span><span class='hs-definition'>unsafeHead</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v} -&gt; Word8 -&gt; Word8</span><span class='hs-varid'>liquidAssert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(Word8 -&gt; Word8) -&gt; Word8 -&gt; Word8</span><span class='hs-varop'>$</span></a>
<span class=hs-linenum>781: </span>  <a class=annot href="#"><span class=annottext>(IO Word8) -&gt; Word8</span><span class='hs-varid'>unsafePerformIO</span></a>  <a class=annot href="#"><span class=annottext>((IO Word8) -&gt; Word8) -&gt; (IO Word8) -&gt; Word8</span><span class='hs-varop'>$</span></a>  <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO Word8))
-&gt; (IO Word8)</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
  -&gt; (IO Word8))
 -&gt; (IO Word8))
-&gt; ({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
    -&gt; (IO Word8))
-&gt; (IO Word8)</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
-&gt; (IO Word8)</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | fplen x == plen VV &amp;&amp; 0 &lt;= plen VV &amp;&amp; l &lt;= plen VV &amp;&amp; s &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>p:(Ptr Word8) -&gt; {v : Int | v &lt; plen p &amp;&amp; 0 &lt;= v} -&gt; (IO Word8)</span><span class='hs-varid'>peekByteOff</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a>
<span class=hs-linenum>782: </span>
<span class=hs-linenum>783: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeTail</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| (bLen v) &gt; 0}</span>
<span class=hs-linenum>784: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| (bLen v) = (bLen b) - 1}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>785: </span><span class='hs-definition'>unsafeTail</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span>
<span class=hs-linenum>786: </span><a class=annot href="#"><span class=annottext>x1:{v : ByteString | bLen v &gt; 0}
-&gt; {v : ByteString | bLen v == bLen x1 - 1}</span><span class='hs-definition'>unsafeTail</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>ps</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v} -&gt; ByteString -&gt; ByteString</span><span class='hs-varid'>liquidAssert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen ps}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(ByteString -&gt; ByteString) -&gt; ByteString -&gt; ByteString</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == ps}</span><span class='hs-varid'>ps</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen ps}</span><span class='hs-varid'>s</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen ps}</span><span class='hs-varid'>l</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>787: </span>
<span class=hs-linenum>788: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>null</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| ((Prop v) &lt;=&gt; ((bLen b) = 0))}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>789: </span><span class='hs-definition'>null</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>790: </span><a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Bool | Prop v &lt;=&gt; bLen x1 == 0}</span><span class='hs-definition'>null</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v} -&gt; Bool -&gt; Bool</span><span class='hs-varid'>liquidAssert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt;= v}</span><span class='hs-varop'>&gt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(Bool -&gt; Bool) -&gt; Bool -&gt; Bool</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>791: </span>
<span class=hs-linenum>792: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeDrop</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span>
<span class=hs-linenum>793: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-keyword'>{v:</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| n &lt;= (bLen v)}</span> 
<span class=hs-linenum>794: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| (bLen v) = (bLen b) - n}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>795: </span><span class='hs-definition'>unsafeDrop</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span>
<span class=hs-linenum>796: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:{v : ByteString | x1 &lt;= bLen v}
-&gt; {v : ByteString | bLen v == bLen x2 - x1}</span><span class='hs-definition'>unsafeDrop</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v} -&gt; ByteString -&gt; ByteString</span><span class='hs-varid'>liquidAssert</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>x1:Bool -&gt; x2:Bool -&gt; {v : Bool | Prop v &lt;=&gt; Prop x1 &amp;&amp; Prop v}</span><span class='hs-varop'>&amp;&amp;</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(ByteString -&gt; ByteString) -&gt; ByteString -&gt; ByteString</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>797: </span>
<span class=hs-linenum>798: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>cons</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| (bLen v) = 1 + (bLen b)}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>799: </span><span class='hs-definition'>cons</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span>
<span class=hs-linenum>800: </span><a class=annot href="#"><span class=annottext>Word8 -&gt; x2:ByteString -&gt; {v : ByteString | bLen v == 1 + bLen x2}</span><span class='hs-definition'>cons</span></a> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>c</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>PS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>unsafeCreate</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | 0 &lt;= plen v &amp;&amp; l &lt;= plen v} -&gt; (IO ()))
 -&gt; {v : ByteString | v /= Memory.empty &amp;&amp; bLen v &gt; 0 &amp;&amp; l &lt;= bLen v})
-&gt; ({v : (Ptr Word8) | 0 &lt;= plen v &amp;&amp; l &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | v /= Memory.empty &amp;&amp; bLen v &gt; 0 &amp;&amp; l &lt;= bLen v}</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV &amp;&amp; l &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
  -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | fplen x == plen VV &amp;&amp; 0 &lt;= plen VV &amp;&amp; l &lt;= plen VV &amp;&amp; s &lt;= plen VV}</span><span class='hs-varid'>f</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>801: </span>        <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == c}</span><span class='hs-varid'>c</span></a>
<span class=hs-linenum>802: </span>        <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; {v : CSize | v &lt;= plen x2 &amp;&amp; v &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-varid'>memcpy</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == f &amp;&amp; fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:Int -&gt; {v : CSize | v == x1}</span><span class='hs-varid'>fromIntegral</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>803: </span>
<span class=hs-linenum>804: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>empty</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| (bLen v) = 0}</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>805: </span><span class='hs-definition'>empty</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span>
<span class=hs-linenum>806: </span><a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == 0}</span><span class='hs-definition'>empty</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>PS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == Memory.nullForeignPtr &amp;&amp; fplen v == 0}</span><span class='hs-varid'>nullForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>807: </span>
<span class=hs-linenum>808: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>assume</span> <span class='hs-varid'>mallocForeignPtrBytes</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>809: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>810: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>malloc</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>811: </span><a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; (IO {VV : (ForeignPtr a) | fplen VV == x1 &amp;&amp; 0 &lt;= fplen VV})</span><span class='hs-definition'>malloc</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> 
<span class=hs-linenum>812: </span>
<span class=hs-linenum>813: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>assume</span>
<span class=hs-linenum>814: </span>    <span class='hs-varid'>c_memcpy</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>dst</span><span class='hs-conop'>:</span><span class='hs-layout'>(</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span>
<span class=hs-linenum>815: </span>             <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>src</span><span class='hs-conop'>:</span><span class='hs-layout'>(</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>816: </span>             <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>size</span><span class='hs-conop'>:</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>CSize</span> <span class='hs-keyword'>| (v &lt;= (plen src) &amp;&amp; v &lt;= (plen dst))}</span> 
<span class=hs-linenum>817: </span>             <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span>
<span class=hs-linenum>818: </span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>819: </span><span class='hs-keyword'>foreign</span> <span class='hs-keyword'>import</span> <span class='hs-keyword'>ccall</span> <span class='hs-keyword'>unsafe</span> <span class='hs-str'>"string.h memcpy"</span> <span class='hs-varid'>c_memcpy</span>
<span class=hs-linenum>820: </span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>CSize</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span>
<span class=hs-linenum>821: </span>
<span class=hs-linenum>822: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>memcpy</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>dst</span><span class='hs-conop'>:</span><span class='hs-layout'>(</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span>
<span class=hs-linenum>823: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>src</span><span class='hs-conop'>:</span><span class='hs-layout'>(</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>824: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>size</span><span class='hs-conop'>:</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>CSize</span> <span class='hs-keyword'>| (v &lt;= (plen src) &amp;&amp; v &lt;= (plen dst))}</span> 
<span class=hs-linenum>825: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span> 
<span class=hs-linenum>826: </span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>827: </span><span class='hs-definition'>memcpy</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>CSize</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>828: </span><a class=annot href="#"><span class=annottext>x1:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; x2:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; {v : CSize | v &lt;= plen x2 &amp;&amp; v &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-definition'>memcpy</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>q</span></a> <a class=annot href="#"><span class=annottext>{v : CSize | v &lt;= plen p &amp;&amp; v &lt;= plen q}</span><span class='hs-varid'>s</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; {v : CSize | v &lt;= plen x1 &amp;&amp; v &lt;= plen x2}
-&gt; (IO (Ptr Word8))</span><span class='hs-varid'>c_memcpy</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == q &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>q</span></a> <a class=annot href="#"><span class=annottext>{v : CSize | v == s &amp;&amp; v &lt;= plen p &amp;&amp; v &lt;= plen q}</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>(IO (Ptr Word8)) -&gt; (IO ()) -&gt; (IO ())</span><span class='hs-varop'>&gt;&gt;</span></a> <a class=annot href="#"><span class=annottext>() -&gt; (IO ())</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
<span class=hs-linenum>829: </span>
<span class=hs-linenum>830: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>assume</span> <span class='hs-varid'>nullForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyword'>| (fplen v) = 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>831: </span><span class='hs-definition'>nullForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>832: </span><a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | fplen v == 0}</span><span class='hs-definition'>nullForeignPtr</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO (ForeignPtr Word8)) -&gt; (ForeignPtr Word8)</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO (ForeignPtr Word8)) -&gt; (ForeignPtr Word8))
-&gt; (IO (ForeignPtr Word8)) -&gt; (ForeignPtr Word8)</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(Ptr Word8)
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == plen x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>newForeignPtr_</span></a> <a class=annot href="#"><span class=annottext>(Ptr Word8)</span><span class='hs-varid'>nullPtr</span></a>
<span class=hs-linenum>833: </span><span class='hs-comment'>{-# NOINLINE nullForeignPtr #-}</span>
</pre>

\end{comment}
