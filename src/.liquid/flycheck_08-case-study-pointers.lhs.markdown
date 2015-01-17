Case Study: Pointers and ByteStrings
====================================

\begin{comment}


<pre><span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diffcheck"</span>     <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span><span class='hs-comment'>{-# LANGUAGE ForeignFunctionInterface #-}</span>
<span class=hs-linenum>11: </span>
<span class=hs-linenum>12: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>Memory</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>13: </span>
<span class=hs-linenum>14: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>null</span><span class='hs-layout'>)</span>
<span class=hs-linenum>15: </span>
<span class=hs-linenum>16: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Word</span>
<span class=hs-linenum>17: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>C</span><span class='hs-varop'>.</span><span class='hs-conid'>Types</span>
<span class=hs-linenum>18: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>ForeignPtr</span>
<span class=hs-linenum>19: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>Ptr</span>
<span class=hs-linenum>20: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Foreign</span><span class='hs-varop'>.</span><span class='hs-conid'>Storable</span>
<span class=hs-linenum>21: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>System</span><span class='hs-varop'>.</span><span class='hs-conid'>IO</span><span class='hs-varop'>.</span><span class='hs-conid'>Unsafe</span>
<span class=hs-linenum>22: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>ByteString</span><span class='hs-varop'>.</span><span class='hs-conid'>Internal</span> <span class='hs-layout'>(</span><span class='hs-varid'>c2w</span><span class='hs-layout'>,</span> <span class='hs-varid'>w2c</span><span class='hs-layout'>)</span>
<span class=hs-linenum>23: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Language</span><span class='hs-varop'>.</span><span class='hs-conid'>Haskell</span><span class='hs-varop'>.</span><span class='hs-conid'>Liquid</span><span class='hs-varop'>.</span><span class='hs-conid'>Prelude</span>
<span class=hs-linenum>24: </span>
<span class=hs-linenum>25: </span><span class='hs-definition'>spanByte</span>         <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteString</span><span class='hs-layout'>,</span> <span class='hs-conid'>ByteString</span><span class='hs-layout'>)</span>
<span class=hs-linenum>26: </span><span class='hs-definition'>unsafeHead</span>       <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>27: </span><span class='hs-definition'>create</span><span class='hs-layout'>,</span> <span class='hs-varid'>create'</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span>
</pre>
\end{comment}

A large part of the allure of Haskell is its elegant, high-level ADTs
that ensure that programs won't be plagued by problems like the infamous
[SSL heartbleed bug](heartbleed.com).\footnotetext{Assuming, of course, the absence of errors in the compiler and run-time...}
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


<pre><span class=hs-linenum>58: </span><span class='hs-definition'>chop</span>     <span class='hs-keyglyph'>::</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span>
<span class=hs-linenum>59: </span><span class='hs-definition'>chop</span> <span class='hs-varid'>s</span> <span class='hs-varid'>n</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>s'</span>
<span class=hs-linenum>60: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>61: </span>    <span class='hs-varid'>b</span>    <span class='hs-keyglyph'>=</span> <span class='hs-conid'>B</span><span class='hs-varop'>.</span><span class='hs-varid'>pack</span> <span class='hs-varid'>s</span>          <span class='hs-comment'>-- down to low-level</span>
<span class=hs-linenum>62: </span>    <span class='hs-varid'>b'</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>B</span><span class='hs-varop'>.</span><span class='hs-varid'>unsafeTake</span> <span class='hs-varid'>n</span> <span class='hs-varid'>b</span>  <span class='hs-comment'>-- grab n chars</span>
<span class=hs-linenum>63: </span>    <span class='hs-varid'>s'</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>B</span><span class='hs-varop'>.</span><span class='hs-varid'>unpack</span> <span class='hs-varid'>b'</span>       <span class='hs-comment'>-- up to high-level</span>
</pre>

\noindent First, the function `pack`s the string into a low-level
bytestring `b`, then it grabs the first `n` `Char`acters from `b`
and translates them back into a high-level `String`. Lets see how
the function works on a small test:


<pre><span class=hs-linenum>72: </span><span class='hs-definition'>ghci</span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>let</span> <span class='hs-varid'>ex</span> <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Ranjit Loves Burritos"</span>
</pre>

\noindent We get the right result when we `chop` a *valid* prefix:


<pre><span class=hs-linenum>78: </span><span class='hs-definition'>ghci</span><span class='hs-varop'>&gt;</span> <span class='hs-varid'>chop</span> <span class='hs-varid'>ex</span> <span class='hs-num'>10</span>
<span class=hs-linenum>79: </span><span class='hs-str'>"Ranjit Lov"</span>
</pre>

\noindent But, as illustrated in Figure~\ref{fig:overflow}, the
machine silently reveals (or more colorfully, *bleeds*) the contents
of adjacent memory or if we use an *invalid* prefix:


<pre><span class=hs-linenum>87: </span><span class='hs-definition'>ghci</span><span class='hs-varop'>&gt;</span> <span class='hs-varid'>heartBleed</span> <span class='hs-varid'>ex</span> <span class='hs-num'>30</span>
<span class=hs-linenum>88: </span><span class='hs-str'>"Ranjit Loves Burritos\NUL\201\&amp;1j\DC3\SOH\NUL"</span>
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
each level such that errors at *each* level are prevented by using the typed
interfaces for the *lower* levels. Next, lets see how this strategy helps develop
a safe means of manipulating pointers.

Low-level Pointer API 
---------------------

To get started, lets look at the low-level pointer API that is
offered by GHC and the run-time. First, lets just see who the
*dramatis personae* are, and how they might let heartbleeds in.
Then, once we have come to grips with the problem, we will see
how to batten down the hatches with LiquidHaskell.

\newthought{Pointers} are an (abstract) type implemented by GHC.
To quote the documentation, "a value of type `Ptr a represents a
pointer to an object, or an array of objects, which may be marshalled
to or from Haskell values of type `a`.


<pre><span class=hs-linenum>128: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span>         
</pre>

\newthought{Foreign Pointers} are *wrapped* pointers that can be
exported to and from C code via the [Foreign Function Interface](foreignptr).


<pre><span class=hs-linenum>135: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> 
</pre>

\newthought{To Create} a pointer we use `mallocForeignPtrBytes n`
which creates a `Ptr` to a buffer of size `n`, wraps it as a
`ForeignPtr` and returns the result:


<pre><span class=hs-linenum>143: </span><span class='hs-definition'>malloc</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span>
</pre>

\newthought{To Unwrap} and actually use the `ForeignPtr` we use 


<pre><span class=hs-linenum>149: </span><span class='hs-definition'>withForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span>     <span class='hs-comment'>-- ^ pointer </span>
<span class=hs-linenum>150: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span>  <span class='hs-comment'>-- ^ action </span>
<span class=hs-linenum>151: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span>             <span class='hs-comment'>-- ^ result</span>
</pre>

\noindent That is, `withForeignPtr fp act` lets us execute a
action `act` on the actual `Ptr` wrapped within the `fp`.
These actions are typically sequences of *dereferences*,
i.e. reads or writes.

\newthought{To Derereference} a pointer, e.g. to read or update
the contents at the corresponding memory location, we use
the functions `peek` and `poke` respectively.
\footnotetext{We elide the `Storable` type class constraint to
strip the presentation down to the absolute essentials.}


<pre><span class=hs-linenum>166: </span><span class='hs-comment'>-- | Read </span>
<span class=hs-linenum>167: </span><span class='hs-definition'>peek</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>a</span>           
<span class=hs-linenum>168: </span>
<span class=hs-linenum>169: </span><span class='hs-comment'>-- | Write</span>
<span class=hs-linenum>170: </span><span class='hs-definition'>poke</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
</pre>

\newthought{For Fine Grained Access} we can directly shift
pointers to arbitrary offsets from the blocks obtained via `malloc`.
This is done via the low-level *pointer arithmetic* operation `plusPtr p off`
which takes a pointer `p` an integer `off` and returns the pointer (address)
obtained shifting `p` by `off`:


<pre><span class=hs-linenum>180: </span><span class='hs-definition'>plusPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>b</span> 
</pre>

\newthought{Example} That was rather dry; lets look at a concrete
example of how one might use the low-level API. The following
function allocates a block of 4 bytes and fills it with zeros:


<pre><span class=hs-linenum>188: </span><a class=annot href="#"><span class=annottext>forall a. (IO (ForeignPtr a))</span><span class='hs-definition'>zero4</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr a) | 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a>
<span class=hs-linenum>189: </span>           <a class=annot href="#"><span class=annottext>x1:(ForeignPtr a)
-&gt; ({v : (Ptr a) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v} -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr a) | fplen fp == plen VV &amp;&amp; zero &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>190: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>191: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>192: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>193: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>194: </span>           <a class=annot href="#"><span class=annottext>(ForeignPtr a) -&gt; (IO (ForeignPtr a))</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a>
<span class=hs-linenum>195: </span>        <span class='hs-keyword'>where</span>
<span class=hs-linenum>196: </span>           <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>zero</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-num'>0</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span>
</pre>

\noindent While the above is perfectly all right, a small typo could
easily slip past the type system (and run-time!) leading to hard to find
errors:


<pre><span class=hs-linenum>204: </span><a class=annot href="#"><span class=annottext>forall a. (IO (ForeignPtr a))</span><span class='hs-definition'>zero4'</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr a) | 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a>
<span class=hs-linenum>205: </span>            <a class=annot href="#"><span class=annottext>x1:(ForeignPtr a)
-&gt; ({v : (Ptr a) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v} -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr a) | fplen fp == plen VV &amp;&amp; zero &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>206: </span>              <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>207: </span>              <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>208: </span>              <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>209: </span>              <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (8  :  int)}</span><span class='hs-num'>8</span></a></span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>210: </span>            <a class=annot href="#"><span class=annottext>(ForeignPtr a) -&gt; (IO (ForeignPtr a))</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a>
<span class=hs-linenum>211: </span>         <span class='hs-keyword'>where</span>
<span class=hs-linenum>212: </span>            <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>zero</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-num'>0</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span>
</pre>

A Refined Pointer API
---------------------

Wouldn't it be great if we had an assistant to helpfully point out
the error above as soon as we *wrote* it? To turn LiquidHaskell into
this friend, we will use the following strategy: 

1. **Refine pointers** with allocated buffer size
2. **Track sizes** in pointer operations

\newthought{To Refining Pointers} with the *size* of their associated
buffers, we can use an *abstract measure*, i.e. a measure specification  
*without* any underlying implementation.

\footnotetext{These two measures, and the signatures for
the associate API are defined and imported from in the
LiquidHaskell [standard library](https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/GHC/Ptr.spec). We include them here for exposition.}


<pre><span class=hs-linenum>234: </span><span class='hs-comment'>-- | Size of `Ptr`</span>
<span class=hs-linenum>235: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>plen</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> 
<span class=hs-linenum>236: </span>
<span class=hs-linenum>237: </span><span class='hs-comment'>-- | Size of `ForeignPtr`</span>
<span class=hs-linenum>238: </span><span class='hs-definition'>measure</span> <span class='hs-varid'>fplen</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> 
</pre>

\noindent As before, it is helpful to define a few
aliases for pointers of a given size `N`:


<pre><span class=hs-linenum>245: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>PtrN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span>        <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span>        <span class='hs-keyglyph'>|</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span>  <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> 
<span class=hs-linenum>246: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> 
</pre>

\newthought{Abstract Measures} are extremely useful when we don't have
a concrete implementation of the underlying value, but we know that
the value *exists*.  \footnotetext{This is another example of a
*ghost* specification} Here, we don't have the value -- inside Haskell
-- because the buffers are manipulated within C. However, this is no
cause for alarm as we will simply use measures to refine the API (not
to perform any computations.)

\newthought{To Refine Allocation} we stipulate that
the size parameter be non-negative, and that the returned
pointer indeed refers to a buffer with exactly `n` bytes:


<pre><span class=hs-linenum>262: </span><span class='hs-definition'>mallocForeignPtrBytes</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span>
</pre>

\newthought{To Refine Unwrapping} we specify that the *action*
gets as input, an unwrapped `Ptr` whose size *equals* that of the
given `ForeignPtr`.


<pre><span class=hs-linenum>270: </span><span class='hs-definition'>withForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>fp</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> 
<span class=hs-linenum>271: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>PtrN</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>fplen</span> <span class='hs-varid'>fp</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span>  
<span class=hs-linenum>272: </span>               <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>b</span>             
</pre>

\noindent This is a rather interesting *higher-order* specification.
Consider a call `withForeignPtr fp act`. If the `act` requires a `Ptr`
whose size *exceeds* that of `fp` then LiquidHaskell will flag a
(subtyping) error indicating the overflow. If instead the `act`
requires a buffer of size less than `fp` then via contra-variant
function subtyping, the input type of `act` will be widened to
the large size, and the code will be accepted.

\newthought{To Refine Reads and Writes} we specify that they can
only be done if the pointer refers to a non-empty (remaining) buffer.
That is, we define an alias:


<pre><span class=hs-linenum>288: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>OkPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-num'>0</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span><span class='hs-layout'>}</span>
</pre>

\noindent that describes pointers referring to *non-empty* buffers
(of strictly positive `plen`), and then use the alias to refine:


<pre><span class=hs-linenum>295: </span><span class='hs-definition'>peek</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>OkPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-varid'>a</span>  
<span class=hs-linenum>296: </span><span class='hs-definition'>poke</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>OkPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>  
</pre>

\noindent In essence the above type says that no matter how arithmetic
was used to shift pointers around, when the actual dereference happens,
the size "remaining" after the pointer must be non-negative (so that a
byte can be safely read from or written to the underlying buffer.)

\newthought{To Refine the Shift} operations, we simply check that the
pointer *remains* within the bounds of the buffer, and update the `plen`
to reflect the size remaining after the shift:
\footnotetext{This signature precludes "left" or "backward" shifts; for
that there is an analogous `minusPtr` which we elide for simplicity}


<pre><span class=hs-linenum>311: </span><span class='hs-definition'>plusPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>off</span><span class='hs-conop'>:</span><span class='hs-conid'>NatLE</span> <span class='hs-layout'>(</span><span class='hs-varid'>plen</span> <span class='hs-varid'>p</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>PtrN</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>plen</span> <span class='hs-varid'>p</span> <span class='hs-comment'>-</span> <span class='hs-varid'>off</span><span class='hs-layout'>)</span>      
</pre>

\noindent using the alias `NatLE`, defined as:


<pre><span class=hs-linenum>317: </span><span class='hs-keyword'>type</span> <span class='hs-conid'>NatLE</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span>
</pre>

\footnotetext{The alert reader will note that we have strengthened
the type of `plusPtr` to prevent the pointer from wandering outside
the boundary of the buffer. We could instead use a weaker requirement
for `plusPtr` that omits this requirement, and instead have the error
be flagged when the pointer was used to read or write memory.}


\newthought{Types Prevent Overflows} Lets revisit the zero-fill example
from above to understand how the refinements help detect the error:


<pre><span class=hs-linenum>331: </span><a class=annot href="#"><span class=annottext>forall a. (IO (ForeignPtr a))</span><span class='hs-definition'>exBad</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr a) | 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (4  :  int)}</span><span class='hs-num'>4</span></a>
<span class=hs-linenum>332: </span>           <a class=annot href="#"><span class=annottext>x1:(ForeignPtr a)
-&gt; ({v : (Ptr a) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v} -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr a) | fplen fp == plen v &amp;&amp; zero &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr a) | fplen fp == plen VV &amp;&amp; zero &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>333: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>334: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>335: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>336: </span>             <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; fplen fp == plen v &amp;&amp; zero &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a></span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == zero}</span><span class='hs-varid'>zero</span></a> 
<span class=hs-linenum>337: </span>           <a class=annot href="#"><span class=annottext>(ForeignPtr a) -&gt; (IO (ForeignPtr a))</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr a) | v == fp &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a>
<span class=hs-linenum>338: </span>        <span class='hs-keyword'>where</span>
<span class=hs-linenum>339: </span>           <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>zero</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-num'>0</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span>
</pre>


\noindent Lets read the tea leaves to understand the above error:

\begin{liquiderror}
  Error: Liquid Type Mismatch
   Inferred type
     VV : {VV : Int | VV == ?a && VV == 5}
  
   not a subtype of Required type
     VV : {VV : Int | VV <= plen p}
  
   in Context
     zero : {zero : Word8 | zero == ?b}
     VV   : {VV : Int | VV == ?a && VV == (5  :  int)}
     fp   : {fp : ForeignPtr a | fplen fp == ?c && 0 <= fplen fp}
     p    : {p  : Ptr a | fplen fp == plen p && ?c <= plen p && ?b <= plen p && zero <= plen p}
     ?a   : {?a : Int | ?a == 5}
     ?c   : {?c : Int | ?c == 4}
     ?b   : {?b : Integer | ?b == 0}
\end{liquiderror}

\noindent The error says we're bumping `p` up by `VV == 5`
using `plusPtr` but the latter *requires* that bump-offset
be within the size of the buffer referred to by `p`, i.e.
`VV <= plen p`. Indeed, in this context, we have:

\begin{liquiderror}
     p    : {p : Ptr a | fplen fp == plen p && ?c <= plen p && ?b <= plen p && zero <= plen p}
     fp   : {fp : ForeignPtr a | fplen fp == ?c && 0 <= fplen fp}
\end{liquiderror}

\noindent that is, the size of `p`, namely `plen p` equals the size of
`fp`, namely `fplen fp` (thanks to the `withForeignPtr` call), and
finally the latter is equal to `?c` which is `4` bytes. Thus, since
the offset `5` is not less than the buffer size `4`, LiquidHaskell
cannot prove that the call to `plusPtr` is safe, hence the error.


Assumptions vs Guarantees
-------------------------

At this point you ought to wonder: where is the *code* for `peek`,
`poke` or `mallocForeignPtrBytes` and so on? How can we know that the
types we assigned to them are in fact legitimate?

\newthought{Frankly, we cannot} as those functions are *externally*
implemented (in this case, in `C`), and hence, invisible to the
otherwise all-seeing eyes of LiquidHaskell. Thus, we are *assuming* or
*trusting* that those functions behave according to their types. Put
another way, the types for the low-level API are our *specification*
for what low-level pointer safety. We shall now *guarantee* that the
higher level modules that build upon this API in fact use the
low-level function in a manner consistent with this specification.

\newthought{Assumptions are a Feature} and not a bug, as they let
us to verify systems that use some modules for which we do not have
the code. Here, we can *assume* a boundary specification, and then
*guarantee* that the rest of the system is safe with respect to
that specification.
\footnotetext{If we so desire, we can also *check* the boundary
specifications at [run-time](http://en.wikipedia.org/wiki/Design_by_contract),
but that is outside the scope of LiquidHaskell}.


ByteString API
--------------

Next, lets see how the low-level API can be used to implement
to implement [ByteStrings][bytestring], in a way that lets us
perform fast string operations without opening the door to
overflows.


\newthought{A ByteString} is implemented as a record


<pre><span class=hs-linenum>418: </span><span class='hs-keyword'>data</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>BS</span> <span class='hs-layout'>{</span>
<span class=hs-linenum>419: </span>    <a class=annot href="#"><span class=annottext>ByteString -&gt; (ForeignPtr Word8)</span><span class='hs-varid'>bPtr</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>420: </span>  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>ByteString -&gt; Int</span><span class='hs-varid'>bOff</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varop'>!</span><span class='hs-conid'>Int</span>
<span class=hs-linenum>421: </span>  <span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Int | v == bLen x1 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>bLen</span></a> <span class='hs-keyglyph'>::</span> <span class='hs-varop'>!</span><span class='hs-conid'>Int</span>
<span class=hs-linenum>422: </span>  <span class='hs-layout'>}</span>
</pre>

\noindent comprising

+ a *pointer* `bPtr` to a contiguous block of memory,
+ an *offset* `bOff` that denotes the position inside
  the block where the string begins, and
+ a *length*  `bLen` that denotes the number of bytes
  (from the offset) that belong to the string.

\begin{figure}[h]
\includegraphics[height=1.0in]{img/bytestring.png}
\caption{Representing ByteStrings in memory.}
\label{fig:bytestring}
\end{figure}

These entities are illustrated in Figure~\ref{fig:bytestring}; the
green portion represents the actual contents of a particular
`ByteString`.  This representation makes it possible to implement
various operations like computing prefixes and suffixes extremely
quickly, simply by pointer arithmetic.

\newthought{In a Legal ByteString} the *start* (`bOff`) and *end*
(`bOff + bLen`) offsets lie inside the buffer referred to by the
pointer `bPtr`. We can formalize this invariant with a data definition
that will then make it impossible to create illegal `ByteString`s: 


<pre><span class=hs-linenum>451: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>BS</span> <span class='hs-layout'>{</span>
<span class=hs-linenum>452: </span>      <span class='hs-varid'>bPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>453: </span>    <span class='hs-layout'>,</span> <span class='hs-varid'>bOff</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span><span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span>        <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>bPtr</span><span class='hs-layout'>}</span>
<span class=hs-linenum>454: </span>    <span class='hs-layout'>,</span> <span class='hs-varid'>bLen</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span><span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>+</span> <span class='hs-varid'>bOff</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>bPtr</span><span class='hs-layout'>}</span>
<span class=hs-linenum>455: </span>    <span class='hs-layout'>}</span>
<span class=hs-linenum>456: </span>  <span class='hs-keyword'>@-}</span>
</pre>

\begin{comment}
-- TODO: we really shouldn't need this...

<pre><span class=hs-linenum>462: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>bLen</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Nat</span> <span class='hs-keyword'>| v = bLen b}</span> <span class='hs-keyword'>@-}</span>
</pre>
\end{comment}

\noindent The refinements on `bOff` and `bLen` correspond exactly
to the legality requirements that the start and end of the `ByteString`
be *within* the block of memory referred to by `bPtr`.


\newthought{For brevity} lets define an alias for `ByteString`s of
a given size:


<pre><span class=hs-linenum>475: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>bLen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\newthought{Legal Bytestrings}  can be created by directly using
the constructor, as long as we pass in suitable offsets and lengths.
For example,


<pre><span class=hs-linenum>483: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>good1</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteStringN</span> <span class='hs-num'>5</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>484: </span><a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == 5})</span><span class='hs-definition'>good1</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a>
<span class=hs-linenum>485: </span>           <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == 5 &amp;&amp; v /= Memory.empty}
-&gt; (IO {v : ByteString | bLen v == 5 &amp;&amp; v /= Memory.empty})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a><span class='hs-layout'>)</span>
</pre>

\noindent creates a valid `ByteString` of size `5`; however we
need not start at the beginning of the block, or use up all
the buffer, and can instead do:


<pre><span class=hs-linenum>493: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>good2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ByteStringN</span> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>494: </span><a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == 2})</span><span class='hs-definition'>good2</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (5  :  int)}</span><span class='hs-num'>5</span></a>
<span class=hs-linenum>495: </span>           <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == 2 &amp;&amp; v /= Memory.empty}
-&gt; (IO {v : ByteString | bLen v == 2 &amp;&amp; v /= Memory.empty})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><span class='hs-layout'>)</span>
</pre>

\noindent Note that the length of `good2` is just `2` which is
*less than* allocated size `5`.

\newthought{Illegal Bytestrings} are rejected by LiquidHaskell.
For example, `bad1`'s length is rather more than the buffer
size, and is flagged as such:


<pre><span class=hs-linenum>506: </span><a class=annot href="#"><span class=annottext>(IO ByteString)</span><span class='hs-definition'>bad1</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> 
<span class=hs-linenum>507: </span>          <a class=annot href="#"><span class=annottext>ByteString -&gt; (IO ByteString)</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (10  :  int)}</span><span class='hs-num'>10</span></a></span><span class='hs-layout'>)</span>
</pre>

\noindent Similarly, `bad2` does have `2` bytes but *not* if
we start at the offset of `2`:


<pre><span class=hs-linenum>514: </span><a class=annot href="#"><span class=annottext>(IO ByteString)</span><span class='hs-definition'>bad2</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | VV /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a>
<span class=hs-linenum>515: </span>          <a class=annot href="#"><span class=annottext>ByteString -&gt; (IO ByteString)</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; v /= Memory.nullForeignPtr &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class='hs-layout'>)</span>
</pre>

\exercisen{Fix the ByteString} Modify the definitions of `bad1`
and `bad2` so they are *accepted* by LiquidHaskell.

\newthought{To Flexibly but Safely Create} a `ByteString` the
implementation defines a higher order `create` function, that
takes a size `n` and accepts a `fill` action, and runs the
action after allocating the pointer. After running the action,
the function tucks the pointer into and returns a `ByteString`
of size `n`.


<pre><span class=hs-linenum>529: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>create</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>n</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>530: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ((Ptr Word8) -&gt; (IO ())) -&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>create</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>(Ptr Word8) -&gt; (IO ())</span><span class='hs-varid'>fill</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == n})
-&gt; {v : ByteString | bLen v == n}</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO {v : ByteString | bLen v == n})
 -&gt; {v : ByteString | bLen v == n})
-&gt; (IO {v : ByteString | bLen v == n})
-&gt; {v : ByteString | bLen v == n}</span><span class='hs-varop'>$</span></a> <span class='hs-keyword'>do</span>
<span class=hs-linenum>531: </span>  <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | fplen VV == n &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a>  <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a>
<span class=hs-linenum>532: </span>  <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; fplen v == n &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>(Ptr Word8) -&gt; (IO ())</span><span class='hs-varid'>fill</span></a> 
<span class=hs-linenum>533: </span>  <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == n}
-&gt; (IO {v : ByteString | bLen v == n})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; fplen v == n &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span>
</pre>

\exercisen{Create} \singlestar Why does LiquidHaskell *reject*
the following function that creates a `ByteString` corresponding
to `"GHC"`?


<pre><span class=hs-linenum>541: </span><a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-definition'>bsGHC</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ((Ptr Word8) -&gt; (IO ())) -&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>create</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (3  :  int)}</span><span class='hs-num'>3</span></a> <a class=annot href="#"><span class=annottext>(((Ptr Word8) -&gt; (IO ())) -&gt; ByteString)
-&gt; ((Ptr Word8) -&gt; (IO ())) -&gt; ByteString</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>(Ptr Word8)</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>542: </span>  <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p}</span><span class='hs-varid'>p</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a></span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Char -&gt; Word8</span><span class='hs-varid'>c2w</span></a> <a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'G'</span></a><span class='hs-layout'>)</span> 
<span class=hs-linenum>543: </span>  <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p}</span><span class='hs-varid'>p</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Char -&gt; Word8</span><span class='hs-varid'>c2w</span></a> <a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'H'</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>544: </span>  <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p}</span><span class='hs-varid'>p</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a></span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Char -&gt; Word8</span><span class='hs-varid'>c2w</span></a> <a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'C'</span></a><span class='hs-layout'>)</span>
</pre>

\hint The function writes into 3 slots starting at `p`.
How big should `plen p` be to allow this? What type
does LiquidHaskell infer for `p` above? Does it meet
the requirement? Which part of the *specification*
or *implementation* needs to be modified so that the
relevant information about `p` becomes available within
the `do`-block above? Make sure you figure out the above
before proceeding.

\newthought{To `pack`} a `String` into a `ByteString`
we simply call `create` with the appropriate fill action:
\footnotetext{The code uses `create'` which is just `create`
with the *correct* signature in case you want to skip the previous
exercise. (But don't!)}


<pre><span class=hs-linenum>563: </span><a class=annot href="#"><span class=annottext>[Char] -&gt; ByteString</span><span class='hs-definition'>pack</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>str</span></a>      <span class='hs-keyglyph'>=</span>  <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>create'</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v == len str}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | plen v == n &amp;&amp; plen v == len str &amp;&amp; plen v == len xs &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}
  -&gt; (IO ()))
 -&gt; ByteString)
-&gt; ({v : (Ptr Word8) | plen v == n &amp;&amp; plen v == len str &amp;&amp; plen v == len xs &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}
    -&gt; (IO ()))
-&gt; ByteString</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | plen v == n &amp;&amp; plen v == len str &amp;&amp; plen v == len xs &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}
-&gt; (IO ())</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | plen VV == n &amp;&amp; plen VV == len str &amp;&amp; plen VV == len xs &amp;&amp; 0 &lt;= plen VV &amp;&amp; n &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | plen v == len str}
-&gt; {v : [Word8] | len v == len str &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; plen v == n &amp;&amp; plen v == len str &amp;&amp; plen v == len xs &amp;&amp; 0 &lt;= plen v &amp;&amp; n &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : [Word8] | v == xs &amp;&amp; len v == len str &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>564: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>565: </span>  <a class=annot href="#"><span class=annottext>{v : Int | v == len str}</span><span class='hs-varid'>n</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == str &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>str</span></a>
<span class=hs-linenum>566: </span>  <a class=annot href="#"><span class=annottext>{v : [Word8] | len v == len str}</span><span class='hs-varid'>xs</span></a>          <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Char -&gt; Word8) -&gt; x3:[Char] -&gt; {v : [Word8] | len v == len x3}</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>Char -&gt; Word8</span><span class='hs-varid'>c2w</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == str &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>str</span></a>
<span class=hs-linenum>567: </span>  <a class=annot href="#"><span class=annottext>forall a.
(Storable a) =&gt;
x1:{VV : (Ptr a) | plen VV == len str}
-&gt; {VV : [a] | len VV == len str &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>(Ptr a)</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | 0 &lt; plen v} -&gt; a -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(IO ()) -&gt; (IO ()) -&gt; (IO ())</span><span class='hs-varop'>&gt;&gt;</span></a> <a class=annot href="#"><span class=annottext>x1:(Ptr a)
-&gt; {VV : [a] | len VV &gt;= 0 &amp;&amp; len VV &lt;= plen x1 &amp;&amp; len VV &lt;= len str}
-&gt; (IO ())</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr a) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>plusPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>568: </span>  <span class='hs-varid'>go</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>() -&gt; (IO ())</span><span class='hs-varid'>return</span></a>  <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
</pre>

\exercisen{Pack} We can compute the size of a `ByteString` by using
the function:

Fix the specification for `pack` so that (it still typechecks!)
and furthermore, the following QuickCheck style *property* is
proved by LiquidHaskell:


<pre><span class=hs-linenum>579: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_pack_length</span>  <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Char</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>580: </span><a class=annot href="#"><span class=annottext>[Char] -&gt; {v : Bool | Prop v}</span><span class='hs-definition'>prop_pack_length</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>xs</span></a>   <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Int | v == bLen x1 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>bLen</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-layout'>(</span></span><span class=hs-error><a class=annot href="#"><span class=annottext>[Char] -&gt; ByteString</span><span class='hs-varid'>pack</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a></span>
</pre>

\hint Look at the type of `length`, and recall that `len`
is a [numeric measure](#numericmeasure) denoting the size
of a list.

\newthought{The magic of inference} ensures that `pack`
just works. Notice there is a tricky little recursive loop
`go` that is used to recursively fill in the `ByteString`
and actually, it has a rather subtle type signature that
LiquidHaskell is able to automatically infer.

\exercise \singlestar Still, we're here to learn, so can you
*write down* the type signature for the loop so that the below
variant of `pack` is accepted by LiquidHaskell (Do this *without*
cheating by peeping at the type inferred for `go` above!)


<pre><span class=hs-linenum>599: </span><a class=annot href="#"><span class=annottext>[Char] -&gt; ByteString</span><span class='hs-definition'>packEx</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>str</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>create'</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v == len str}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>(((Ptr Word8) -&gt; (IO ())) -&gt; ByteString)
-&gt; ((Ptr Word8) -&gt; (IO ())) -&gt; ByteString</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>(Ptr Word8) -&gt; (IO ())</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>(Ptr Word8)</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>forall a. (Storable a) =&gt; (Ptr a) -&gt; [a] -&gt; (IO ())</span><span class='hs-varid'>pLoop</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : [Word8] | v == xs &amp;&amp; len v == len str &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>600: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>601: </span>  <a class=annot href="#"><span class=annottext>{v : Int | v == len str}</span><span class='hs-varid'>n</span></a>            <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == str &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>str</span></a>
<span class=hs-linenum>602: </span>  <a class=annot href="#"><span class=annottext>{v : [Word8] | len v == len str}</span><span class='hs-varid'>xs</span></a>           <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(Char -&gt; Word8) -&gt; x3:[Char] -&gt; {v : [Word8] | len v == len x3}</span><span class='hs-varid'>map</span></a> <a class=annot href="#"><span class=annottext>Char -&gt; Word8</span><span class='hs-varid'>c2w</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == str &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>str</span></a>
<span class=hs-linenum>603: </span>
<span class=hs-linenum>604: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>pLoop</span>      <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Storable</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>605: </span><a class=annot href="#"><span class=annottext>forall a. (Storable a) =&gt; (Ptr a) -&gt; [a] -&gt; (IO ())</span><span class='hs-definition'>pLoop</span></a> <a class=annot href="#"><span class=annottext>(Ptr a)</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | 0 &lt; plen v} -&gt; a -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p}</span><span class='hs-varid'>p</span></a></span> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(IO ()) -&gt; (IO ()) -&gt; (IO ())</span><span class='hs-varop'>&gt;&gt;</span></a> <a class=annot href="#"><span class=annottext>forall a. (Storable a) =&gt; (Ptr a) -&gt; [a] -&gt; (IO ())</span><span class='hs-varid'>pLoop</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : (Ptr a) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr a) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>plusPtr</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p}</span><span class='hs-varid'>p</span></a></span> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a></span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>606: </span><span class='hs-definition'>pLoop</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>() -&gt; (IO ())</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
</pre>

\hint Remember that `len xs` denotes the size of the list `xs`.

\exercisen{`unsafeTake` and `unsafeDrop`} respectively extract
the prefix and suffix of a `ByteString` from a given position.
They are really fast since we only have to change the offsets.
But why does LiquidHaskell reject them? Can you fix the
specifications so that they are accepted?


<pre><span class=hs-linenum>618: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeTake</span>          <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>n</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>619: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ByteString -&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>unsafeTake</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a></span>
<span class=hs-linenum>620: </span>
<span class=hs-linenum>621: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeDrop</span>          <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-keyword'>{bLen b - n}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>622: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:ByteString -&gt; {v : ByteString | bLen v == bLen x2 - x1}</span><span class='hs-definition'>unsafeDrop</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a></span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a></span><span class='hs-layout'>)</span>
</pre>

\hint Under what conditions are the returned `ByteString`s legal?


\newthought{To `unpack`} a `ByteString` into a plain old `String`,
we essentially run `pack` in reverse, by walking over the pointer,
and reading out the characters one by one till we reach the end:


<pre><span class=hs-linenum>633: </span><span class='hs-definition'>unpack</span>              <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> 
<span class=hs-linenum>634: </span><a class=annot href="#"><span class=annottext>ByteString -&gt; [Char]</span><span class='hs-definition'>unpack</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-keyword'>_</span>  <span class='hs-keyword'>_</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | null v &lt;=&gt; true &amp;&amp; bLens v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>635: </span><span class='hs-definition'>unpack</span> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>ps</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO [Char]) -&gt; [Char]</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO [Char]) -&gt; [Char]) -&gt; (IO [Char]) -&gt; [Char]</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO [Char]))
-&gt; (IO [Char])</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>(ForeignPtr Word8)</span><span class='hs-varid'>ps</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | 0 &lt;= plen v} -&gt; (IO [Char])) -&gt; (IO [Char]))
-&gt; ({v : (Ptr Word8) | 0 &lt;= plen v} -&gt; (IO [Char])) -&gt; (IO [Char])</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>636: </span>    <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen x1}
-&gt; {v : [Char] | len v == 0 &amp;&amp; len v &lt;= plen x1}
-&gt; (IO [Char])</span><span class='hs-varid'>go</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; true &amp;&amp; bLens v == 0 &amp;&amp; len v == 0 &amp;&amp; len v &gt;= 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>637: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>638: </span>    <span class='hs-keyword'>{-@</span> <span class='hs-varid'>go</span>     <span class='hs-keyglyph'>::</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>acc</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-keyword'>{v:</span><span class='hs-keyword'>_</span> <span class='hs-keyword'>| true }</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>639: </span>    <a class=annot href="#"><span class=annottext>x1:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; {VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= plen x1}
-&gt; {VV : [Char] | len VV == 0 &amp;&amp; len VV &lt;= plen x1}
-&gt; (IO [Char])</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-num'>0</span> <a class=annot href="#"><span class=annottext>{VV : [Char] | len VV &gt;= 0}</span><span class='hs-varid'>acc</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt; plen v}
-&gt; (IO {v : Word8 | v == deref x1})</span><span class='hs-varid'>peek</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>(IO Word8) -&gt; (Word8 -&gt; (IO [Char])) -&gt; (IO [Char])</span><span class='hs-varop'>&gt;&gt;=</span></a> <a class=annot href="#"><span class=annottext>Word8 -&gt; (IO [Char])</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>e</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; (IO [Char])</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Word8 -&gt; Char</span><span class='hs-varid'>w2c</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == e}</span><span class='hs-varid'>e</span></a> <a class=annot href="#"><span class=annottext>x1:Char
-&gt; x2:[Char]
-&gt; {v : [Char] | null v &lt;=&gt; false &amp;&amp; bLens v == bLen x1 + bLens x2 &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == acc &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>acc</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>640: </span>    <span class='hs-varid'>go</span> <span class='hs-varid'>p</span> <span class='hs-varid'>n</span> <span class='hs-varid'>acc</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt; plen v}
-&gt; (IO {v : Word8 | v == deref x1})</span><span class='hs-varid'>peek</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen p}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(IO Word8) -&gt; (Word8 -&gt; (IO [Char])) -&gt; (IO [Char])</span><span class='hs-varop'>&gt;&gt;=</span></a>  <a class=annot href="#"><span class=annottext>Word8 -&gt; (IO [Char])</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>e</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; {VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= plen x1}
-&gt; {VV : [Char] | len VV &gt;= 0}
-&gt; (IO [Char])</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0 &amp;&amp; v &lt;= plen p}</span><span class='hs-varid'>n</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Word8 -&gt; Char</span><span class='hs-varid'>w2c</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == e}</span><span class='hs-varid'>e</span></a> <a class=annot href="#"><span class=annottext>x1:Char
-&gt; x2:[Char]
-&gt; {v : [Char] | null v &lt;=&gt; false &amp;&amp; bLens v == bLen x1 + bLens x2 &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == acc &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>acc</span></a><span class='hs-layout'>)</span>
</pre>

\exercisen{Unpack} \singlestar Fix the specification for `unpack`
so that the below QuickCheck style property is proved by LiquidHaskell.


<pre><span class=hs-linenum>647: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_unpack_length</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>648: </span><a class=annot href="#"><span class=annottext>ByteString -&gt; {v : Bool | Prop v}</span><span class='hs-definition'>prop_unpack_length</span></a> <a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-varid'>b</span></a>   <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Int | v == bLen x1 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>bLen</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : ByteString | v == b}</span><span class='hs-varid'>b</span></a></span><span class=hs-error>  </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-layout'>(</span></span><span class=hs-error><a class=annot href="#"><span class=annottext>ByteString -&gt; [Char]</span><span class='hs-varid'>unpack</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : ByteString | v == b}</span><span class='hs-varid'>b</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span>
</pre>

\hint You will also have to fix the specification of the helper `go`.
Can you determine the output refinement should be (instead of just `true`?)
How *big* is the output list in terms of `p`, `n` and `acc`.


Application API 
---------------

Finally, lets revisit our potentially  "bleeding" `chop` function to see
how the refined `ByteString` API can prevent errors.

\begin{comment}

<pre><span class=hs-linenum>664: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>StringN</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>String</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>len</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>665: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>NatLE</span> <span class='hs-conid'>N</span>   <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span>    <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span>    <span class='hs-keyword'>@-}</span>
</pre>
\end{comment}

\noindent The signature specifies that the prefix size `n` must be less than
the size of the input string `s`.


<pre><span class=hs-linenum>673: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>chop</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>s</span><span class='hs-conop'>:</span><span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>NatLE</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>s</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>674: </span><a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1} -&gt; [Char]</span><span class='hs-definition'>chop</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0 &amp;&amp; v &lt;= len s}</span><span class='hs-varid'>n</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | v == s' &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>s'</span></a>
<span class=hs-linenum>675: </span>  <span class='hs-keyword'>where</span> 
<span class=hs-linenum>676: </span>    <a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-varid'>b</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[Char] -&gt; ByteString</span><span class='hs-varid'>pack</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == s &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>s</span></a>          <span class='hs-comment'>-- down to low-level</span>
<span class=hs-linenum>677: </span>    <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == n}</span><span class='hs-varid'>b'</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ByteString -&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>unsafeTake</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= len s}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == b &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>b</span></a>  <span class='hs-comment'>-- grab n chars</span>
<span class=hs-linenum>678: </span>    <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>s'</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>ByteString -&gt; [Char]</span><span class='hs-varid'>unpack</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == b' &amp;&amp; bLen v == n &amp;&amp; bLen v &gt;= 0}</span><span class='hs-varid'>b'</span></a>       <span class='hs-comment'>-- up to high-level</span>
</pre>

\newthought{Overflows are prevented} by LiquidHaskell, as it
rejects calls to `chop` where the prefix size is too large
(which is what led to the overflow that spilled the contents
of memory after the string, as illustrated in Figure~\ref{fig:overflow}).
Thus, in the code below, the first use of `chop` which defines `ex6` is accepted
as `6 <= len ex` but the second call is rejected because `30 > len ex`.


<pre><span class=hs-linenum>689: </span><a class=annot href="#"><span class=annottext>[[Char]]</span><span class='hs-definition'>demo</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [[Char]] | null v &lt;=&gt; false &amp;&amp; xListSelector v == ex30 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>{v : [Char] | v == ex6 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex6</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : [Char] | v == ex30 &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex30</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>690: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>691: </span>    <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false}</span><span class='hs-varid'>ex</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false}</span><span class='hs-keyglyph'>[</span></a><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'L'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'I'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'Q'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'U'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'I'</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>Char</span><span class='hs-chr'>'D'</span></a><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>692: </span>    <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>ex6</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1} -&gt; [Char]</span><span class='hs-varid'>chop</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false &amp;&amp; v == ex &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (6  :  int)}</span><span class='hs-num'>6</span></a>   <span class='hs-comment'>-- accepted by LH </span>
<span class=hs-linenum>693: </span>    <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>ex30</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1} -&gt; [Char]</span><span class='hs-varid'>chop</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | null v &lt;=&gt; false &amp;&amp; v == ex &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>ex</span></a> <span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == (30  :  int)}</span><span class='hs-num'>30</span></a></span>  <span class='hs-comment'>-- rejected by LH </span>
</pre>

\exercisen{Chop} Fix the specification for `chop` so that
the following property is proved:


<pre><span class=hs-linenum>700: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>prop_chop_length</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>701: </span><a class=annot href="#"><span class=annottext>[Char] -&gt; {v : Int | v &gt;= 0} -&gt; {v : Bool | Prop v}</span><span class='hs-definition'>prop_chop_length</span></a> <a class=annot href="#"><span class=annottext>[Char]</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a>
<span class=hs-linenum>702: </span>  <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &lt;= v}</span><span class='hs-varop'>&lt;=</span></a> <a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>{v : [Char] | v == s &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>s</span></a>     <span class='hs-keyglyph'>=</span> <span class=hs-error><a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v == len x1}</span><span class='hs-varid'>length</span></a></span><span class=hs-error> </span><span class=hs-error><span class='hs-layout'>(</span></span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:[Char] -&gt; {v : Int | v &gt;= 0 &amp;&amp; v &lt;= len x1} -&gt; [Char]</span><span class='hs-varid'>chop</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : [Char] | v == s &amp;&amp; len v &gt;= 0 &amp;&amp; bLens v &gt;= 0}</span><span class='hs-varid'>s</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a></span><span class=hs-error><span class='hs-layout'>)</span></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a></span><span class=hs-error> </span><span class=hs-error><a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a></span>
<span class=hs-linenum>703: </span>  <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Bool | Prop v &amp;&amp; v == GHC.Types.True}</span><span class='hs-conid'>True</span></a>
</pre>

Nested ByteStrings 
------------------

For a more in-depth example, let's take a look at `group`,
which transforms strings like


<pre><span class=hs-linenum>713: </span><span class='hs-varop'>`"foobaaar"`</span>
</pre>

into *lists* of strings like


<pre><span class=hs-linenum>719: </span><span class='hs-varop'>`</span><span class='hs-keyglyph'>[</span><span class='hs-str'>"f"</span><span class='hs-layout'>,</span><span class='hs-str'>"oo"</span><span class='hs-layout'>,</span> <span class='hs-str'>"b"</span><span class='hs-layout'>,</span> <span class='hs-str'>"aaa"</span><span class='hs-layout'>,</span> <span class='hs-str'>"r"</span><span class='hs-keyglyph'>]</span><span class='hs-varop'>`</span><span class='hs-varop'>.</span>
</pre>

The specification is that `group` should produce a

1. list of *non-empty* `ByteStrings`, 
2. the *sum of* whose lengths equals that of the input string.

\newthought{Non-empty ByteStrings} are those whose length is non-zero:


<pre><span class=hs-linenum>730: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ByteStringNE</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>bLen</span> <span class='hs-varid'>v</span> <span class='hs-varop'>/=</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

\noindent We can use these to define enrich the ByteString API with a `null` check


<pre><span class=hs-linenum>736: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>null</span>               <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>Bool</span> <span class='hs-keyword'>| Prop v &lt;=&gt; bLen b == 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>737: </span><a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Bool | Prop v &lt;=&gt; bLen x1 == 0}</span><span class='hs-definition'>null</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 == v}</span><span class='hs-varop'>==</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
</pre>

\noindent This check is used to determine if it is safe to extract
the head and tail of the `ByteString`. Notice how we can use refinements
to ensure the safety of the operations, and also track the sizes.
\footnotetext{`peekByteOff p i` is equivalent to `peek (plusPtr p i)`}


<pre><span class=hs-linenum>746: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeHead</span>        <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteStringNE</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Word8</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>747: </span><a class=annot href="#"><span class=annottext>{v : ByteString | bLen v /= 0} -&gt; Word8</span><span class='hs-definition'>unsafeHead</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO Word8) -&gt; Word8</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO Word8) -&gt; Word8) -&gt; (IO Word8) -&gt; Word8</span><span class='hs-varop'>$</span></a>
<span class=hs-linenum>748: </span>                          <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO Word8))
-&gt; (IO Word8)</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; s &lt;= plen v}
  -&gt; (IO Word8))
 -&gt; (IO Word8))
-&gt; ({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; s &lt;= plen v}
    -&gt; (IO Word8))
-&gt; (IO Word8)</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | fplen x == plen VV &amp;&amp; 0 &lt;= plen VV &amp;&amp; s &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>749: </span>                            <a class=annot href="#"><span class=annottext>p:(Ptr Word8) -&gt; {v : Int | v &lt; plen p &amp;&amp; 0 &lt;= v} -&gt; (IO Word8)</span><span class='hs-varid'>peekByteOff</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; s &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a>
<span class=hs-linenum>750: </span>
<span class=hs-linenum>751: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeTail</span>         <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteStringNE</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-keyword'>{bLen b - 1}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>752: </span><a class=annot href="#"><span class=annottext>x1:{v : ByteString | bLen v /= 0}
-&gt; {v : ByteString | bLen v == bLen x1 - 1}</span><span class='hs-definition'>unsafeTail</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>ps</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == ps}</span><span class='hs-varid'>ps</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen ps}</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen ps}</span><span class='hs-varid'>l</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span>
</pre>

\newthought{The `group`} function recursively calls `spanByte` to carve off
the next group, and then returns the accumulated results:


<pre><span class=hs-linenum>759: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>group</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>ByteStringNE</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>| bLens v = bLen b}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>760: </span><a class=annot href="#"><span class=annottext>x1:ByteString
-&gt; {v : [{v : ByteString | bLen v /= 0}] | bLens v == bLen x1}</span><span class='hs-definition'>group</span></a> <a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>761: </span>    <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Bool | Prop v &lt;=&gt; bLen x1 == 0}</span><span class='hs-varid'>null</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs}</span><span class='hs-varid'>xs</span></a>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x5 VV -&gt; p x5&gt; | null v &lt;=&gt; true &amp;&amp; bLens v == 0 &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>762: </span>    <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>let</span>  <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>y</span></a>        <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v /= 0} -&gt; Word8</span><span class='hs-varid'>unsafeHead</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>763: </span>                       <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : ByteString | VV == ys}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{VV : ByteString | VV == zs}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>Word8
-&gt; x2:ByteString
-&gt; {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen x2}</span><span class='hs-varid'>spanByte</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == y}</span><span class='hs-varid'>y</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:{v : ByteString | bLen v /= 0}
-&gt; {v : ByteString | bLen v == bLen x1 - 1}</span><span class='hs-varid'>unsafeTail</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == xs}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>764: </span>                  <span class='hs-keyword'>in</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Word8 | v == y}</span><span class='hs-varid'>y</span></a> <a class=annot href="#"><span class=annottext>Word8
-&gt; x2:ByteString
-&gt; {v : ByteString | bLen v == bLen x2 + 1 &amp;&amp; bLen v /= 0}</span><span class='hs-varop'>`cons`</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == ys &amp;&amp; v == ys}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:{v : ByteString | v /= Memory.empty &amp;&amp; bLen v /= 0}
-&gt; x2:[{v : ByteString | v /= Memory.empty &amp;&amp; bLen v /= 0}]&lt;\_ VV -&gt; v /= Memory.empty &amp;&amp; bLen v /= 0&gt;
-&gt; {v : [{v : ByteString | v /= Memory.empty &amp;&amp; bLen v /= 0}]&lt;\_ VV -&gt; v /= Memory.empty &amp;&amp; bLen v /= 0&gt; | null v &lt;=&gt; false &amp;&amp; bLens v == bLen x1 + bLens x2 &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>x1:ByteString
-&gt; {v : [{v : ByteString | bLen v /= 0}] | bLens v == bLen x1}</span><span class='hs-varid'>group</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == zs &amp;&amp; v == zs}</span><span class='hs-varid'>zs</span></a>
</pre>

\noindent The first requirement, that the groups be non-empty is captured by the fact that
the output is a `[ByteStringNE]`. The second requirement, that the sum of the lengths is
preserved, is expressed by a writing a [numeric measure](#numericmeasure):


<pre><span class=hs-linenum>772: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>measure</span> <span class='hs-varid'>bLens</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>773: </span><span class='hs-definition'>bLens</span>        <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>ByteString</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>774: </span><a class=annot href="#"><span class=annottext>x1:[ByteString] -&gt; {VV : Int | VV == bLens x1}</span><span class='hs-definition'>bLens</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int# -&gt; {v : Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>775: </span><span class='hs-definition'>bLens</span> <span class='hs-layout'>(</span><span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-varid'>bs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:ByteString -&gt; {v : Int | v == bLen x1 &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>bLen</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == b}</span><span class='hs-varid'>b</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>x1:[ByteString] -&gt; {VV : Int | VV == bLens x1}</span><span class='hs-varid'>bLens</span></a> <a class=annot href="#"><span class=annottext>{v : [ByteString] | v == bs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>bs</span></a>
</pre>


\newthought{`spanByte`} does a lot of the heavy lifting. It uses low-level pointer
arithmetic to find the *first* position in the `ByteString` that is different from
the input character `c` and then splits the `ByteString` into a pair comprising the
prefix and suffix at that point.


<pre><span class=hs-linenum>785: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>spanByte</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString2</span> <span class='hs-varid'>b</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>786: </span><a class=annot href="#"><span class=annottext>Word8
-&gt; x2:ByteString
-&gt; {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen x2}</span><span class='hs-definition'>spanByte</span></a> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>ByteString</span><span class='hs-varid'>ps</span></a><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})
-&gt; {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})
 -&gt; {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})
-&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})
-&gt; {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}))
-&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; s &lt;= plen v &amp;&amp; l &lt;= plen v}
  -&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}))
 -&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}))
-&gt; ({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; s &lt;= plen v &amp;&amp; l &lt;= plen v}
    -&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}))
-&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | fplen x == plen VV &amp;&amp; 0 &lt;= plen VV &amp;&amp; s &lt;= plen VV &amp;&amp; l &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>787: </span>    <a class=annot href="#"><span class=annottext>{v : (Ptr (Any *)) | l &lt;= plen v}
-&gt; {v : Int | v == 0 &amp;&amp; v == bLen Memory.empty &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= s &amp;&amp; v &lt;= l}
-&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})</span><span class='hs-varid'>go</span></a>  <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; s &lt;= plen v &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr (Any *)) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>788: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>789: </span>    <a class=annot href="#"><span class=annottext>forall a.
{VV : (Ptr a) | l &lt;= plen VV}
-&gt; {VV : Int | VV == 0 &amp;&amp; VV == bLen Memory.empty &amp;&amp; VV &gt;= 0 &amp;&amp; VV &lt;= s &amp;&amp; VV &lt;= l}
-&gt; (IO {VV : (ByteString, ByteString) | bLen first VV + bLen second VV == bLen ps})</span><span class='hs-varid'>go</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr a) | l &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= l}</span><span class='hs-varid'>i</span></a> <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Bool | Prop v &lt;=&gt; x1 &gt;= v}</span><span class='hs-varop'>&gt;=</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}
-&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : (ByteString, ByteString) | second v == Memory.empty &amp;&amp; x_Tuple22 v == Memory.empty &amp;&amp; snd v == Memory.empty}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{v : ByteString | v == ps &amp;&amp; v == Memory.BS x s l &amp;&amp; bLen v == l &amp;&amp; bOff v == s &amp;&amp; bPtr v == x}</span><span class='hs-varid'>ps</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>{v : ByteString | v == Memory.empty &amp;&amp; bLen v == 0}</span><span class='hs-varid'>empty</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>790: </span>           <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>c'</span></a> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>p:(Ptr a) -&gt; {v : Int | v &lt; plen p &amp;&amp; 0 &lt;= v} -&gt; (IO Word8)</span><span class='hs-varid'>peekByteOff</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a>
<span class=hs-linenum>791: </span>                            <span class='hs-keyword'>if</span> <a class=annot href="#"><span class=annottext>{v : Word8 | v == c}</span><span class='hs-varid'>c</span></a> <a class=annot href="#"><span class=annottext>x1:Word8 -&gt; x2:Word8 -&gt; {v : Bool | Prop v &lt;=&gt; x1 /= v}</span><span class='hs-varop'>/=</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == c'}</span><span class='hs-varid'>c'</span></a>
<span class=hs-linenum>792: </span>                                <span class='hs-keyword'>then</span> <a class=annot href="#"><span class=annottext>{v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps}
-&gt; (IO {v : (ByteString, ByteString) | bLen first v + bLen second v == bLen ps})</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>(ByteString, ByteString)</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ByteString -&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>unsafeTake</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == ps &amp;&amp; v == Memory.BS x s l &amp;&amp; bLen v == l &amp;&amp; bOff v == s &amp;&amp; bPtr v == x}</span><span class='hs-varid'>ps</span></a><span class='hs-layout'>,</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; x2:ByteString -&gt; {v : ByteString | bLen v == bLen v - x1}</span><span class='hs-varid'>unsafeDrop</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a> <a class=annot href="#"><span class=annottext>{v : ByteString | v == ps &amp;&amp; v == Memory.BS x s l &amp;&amp; bLen v == l &amp;&amp; bOff v == s &amp;&amp; bPtr v == x}</span><span class='hs-varid'>ps</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>793: </span>                                <span class='hs-keyword'>else</span> <a class=annot href="#"><span class=annottext>{VV : (Ptr a) | l &lt;= plen VV}
-&gt; {VV : Int | VV &gt;= 0 &amp;&amp; VV &lt;= l}
-&gt; (IO {VV : (ByteString, ByteString) | bLen first VV + bLen second VV == bLen ps})</span><span class='hs-varid'>go</span></a>  <a class=annot href="#"><span class=annottext>{v : (Ptr a) | v == p &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == i &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= l}</span><span class='hs-varid'>i</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span>
</pre>

LiquidHaskell infers that `0 <= i <= l` and therefore that
all of the memory accesses are safe. Furthermore, due to
the precise specifications given to `unsafeTake` and
`unsafeDrop`, it is able to prove that the output pair's
lengths add up to the size of the input `ByteString`.


<pre><span class=hs-linenum>803: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ByteString2</span> <span class='hs-conid'>B</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>bLen</span> <span class='hs-layout'>(</span><span class='hs-varid'>fst</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varop'>+</span> <span class='hs-varid'>bLen</span> <span class='hs-layout'>(</span><span class='hs-varid'>snd</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>bLen</span> <span class='hs-conid'>B</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

Recap: Types Against Overflows
------------------------------

In this chapter we saw a case study illustrating how measures and refinements
enable safe low-level pointer arithmetic in Haskell. The take away messages are:

1. larger systems are *composed of* layers of smaller ones,
2. we can write *refined APIs* for each layer,
3. that can be used to inform the *design* and
   ensure *correctness* of the layers above.

We saw this in action by developing a low-level `Pointer` API, using it to
implement fast `ByteString`s API, and then building some higher-level
functions on top of the `ByteStrings`.

\newthought{The Trusted Computing Base} in this approach includes
exactly those layers for which the code is *not* available, for
example, because they are implemented outside the language and
accessed via the FFI as with `mallocForeignPtrBytes` and `peek` and
`poke`. In this case, we can make progress by *assuming* the APIs hold
for those layers and verify the rest of the system with respect to
that API.  It is important to note that in the entire case study, it
is only the above FFI signatures that are *trusted*; the rest are all
verified by LiquidHaskell.

\begin{comment}

<pre><span class=hs-linenum>833: </span><span class='hs-comment'>-----------------------------------------------------------------------</span>
<span class=hs-linenum>834: </span><span class='hs-comment'>-- Helper Code</span>
<span class=hs-linenum>835: </span><span class='hs-comment'>-----------------------------------------------------------------------</span>
<span class=hs-linenum>836: </span>
<span class=hs-linenum>837: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>unsafeCreate</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>l</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>PtrN</span> <span class='hs-conid'>Word8</span> <span class='hs-varid'>l</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>l</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>838: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({VV : (Ptr Word8) | plen VV == x1 &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>unsafeCreate</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | plen VV == n &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ())</span><span class='hs-varid'>f</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>create'</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | plen v == n &amp;&amp; 0 &lt;= plen v} -&gt; (IO ())</span><span class='hs-varid'>f</span></a> <span class='hs-comment'>-- unsafePerformIO $ create n f</span>
<span class=hs-linenum>839: </span>
<span class=hs-linenum>840: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>PLLen</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>p</span><span class='hs-conop'>:</span><span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>len</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>p</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>841: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>ForeignPtrN</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-varid'>int</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>n</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>842: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>FPLenPLen</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>fp</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>fp</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>843: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>PtrLen</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-conid'>List</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>len</span> <span class='hs-varid'>xs</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>844: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>qualif</span> <span class='hs-conid'>PlenEq</span><span class='hs-layout'>(</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-conid'>Ptr</span> <span class='hs-varid'>a</span><span class='hs-layout'>,</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span> <span class='hs-varid'>int</span><span class='hs-layout'>)</span><span class='hs-conop'>:</span> <span class='hs-varid'>x</span> <span class='hs-varop'>&lt;=</span> <span class='hs-varid'>plen</span> <span class='hs-varid'>v</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>845: </span>
<span class=hs-linenum>846: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>cons</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteStringNE</span> <span class='hs-keyword'>| bLen v = bLen b + 1}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>847: </span><span class='hs-definition'>cons</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteString</span>
<span class=hs-linenum>848: </span><a class=annot href="#"><span class=annottext>Word8
-&gt; x2:ByteString
-&gt; {v : ByteString | bLen v == bLen x2 + 1 &amp;&amp; bLen v /= 0}</span><span class='hs-definition'>cons</span></a> <a class=annot href="#"><span class=annottext>Word8</span><span class='hs-varid'>c</span></a> <span class='hs-layout'>(</span><span class='hs-conid'>BS</span> <span class='hs-varid'>x</span> <span class='hs-varid'>s</span> <span class='hs-varid'>l</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({v : (Ptr Word8) | plen v == x1 &amp;&amp; 0 &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-varid'>unsafeCreate</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | 0 &lt;= plen v &amp;&amp; l &lt;= plen v} -&gt; (IO ()))
 -&gt; {v : ByteString | v /= Memory.empty &amp;&amp; bLen v /= 0 &amp;&amp; l &lt;= bLen v})
-&gt; ({v : (Ptr Word8) | 0 &lt;= plen v &amp;&amp; l &lt;= plen v} -&gt; (IO ()))
-&gt; {v : ByteString | v /= Memory.empty &amp;&amp; bLen v /= 0 &amp;&amp; l &lt;= bLen v}</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV &amp;&amp; l &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>(({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
  -&gt; (IO ()))
 -&gt; (IO ()))
-&gt; ({v : (Ptr Word8) | fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | fplen x == plen VV &amp;&amp; 0 &lt;= plen VV &amp;&amp; l &lt;= plen VV &amp;&amp; s &lt;= plen VV}</span><span class='hs-varid'>f</span></a> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>849: </span>        <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | 0 &lt; plen v} -&gt; Word8 -&gt; (IO ())</span><span class='hs-varid'>poke</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : Word8 | v == c}</span><span class='hs-varid'>c</span></a>
<span class=hs-linenum>850: </span>        <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; {v : CSize | v &lt;= plen x1 &amp;&amp; v &lt;= plen x2}
-&gt; (IO ())</span><span class='hs-varid'>memcpy</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == f &amp;&amp; fplen x == plen v &amp;&amp; 0 &lt;= plen v &amp;&amp; l &lt;= plen v &amp;&amp; s &lt;= plen v}</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : Int | v &lt;= plen x1}
-&gt; {v : (Ptr Word8) | pbase v == pbase x1 &amp;&amp; plen v == plen x1 - x2 &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varop'>`plusPtr`</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == s &amp;&amp; v &gt;= 0 &amp;&amp; v &lt;= fplen x}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:Int -&gt; {v : CSize | v == x1}</span><span class='hs-varid'>fromIntegral</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == l &amp;&amp; v &gt;= 0 &amp;&amp; v + s &lt;= fplen x}</span><span class='hs-varid'>l</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>851: </span>
<span class=hs-linenum>852: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>empty</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span><span class='hs-conid'>ByteString</span> <span class='hs-keyword'>| bLen v = 0}</span> <span class='hs-keyword'>@-}</span> 
<span class=hs-linenum>853: </span><span class='hs-definition'>empty</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ByteString</span>
<span class=hs-linenum>854: </span><a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == 0}</span><span class='hs-definition'>empty</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == Memory.nullForeignPtr &amp;&amp; fplen v == 0}</span><span class='hs-varid'>nullForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>855: </span>
<span class=hs-linenum>856: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>assume</span> <span class='hs-varid'>mallocForeignPtrBytes</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>857: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-conid'>ForeignPtr</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>fplen</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>858: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>malloc</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>ForeignPtrN</span> <span class='hs-varid'>a</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>859: </span><a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; (IO {VV : (ForeignPtr a) | fplen VV == x1 &amp;&amp; 0 &lt;= fplen VV})</span><span class='hs-definition'>malloc</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr a) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> 
<span class=hs-linenum>860: </span>
<span class=hs-linenum>861: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>assume</span>
<span class=hs-linenum>862: </span>    <span class='hs-varid'>c_memcpy</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>dst</span><span class='hs-conop'>:</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>863: </span>             <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>src</span><span class='hs-conop'>:</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span> 
<span class=hs-linenum>864: </span>             <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>size</span><span class='hs-conop'>:</span><span class='hs-keyword'>{CSize | size &lt;= plen src &amp;&amp; size &lt;= plen dst}</span> 
<span class=hs-linenum>865: </span>             <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span>
<span class=hs-linenum>866: </span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>867: </span><span class='hs-keyword'>foreign</span> <span class='hs-keyword'>import</span> <span class='hs-keyword'>ccall</span> <span class='hs-keyword'>unsafe</span> <span class='hs-str'>"string.h memcpy"</span> <span class='hs-varid'>c_memcpy</span>
<span class=hs-linenum>868: </span>    <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>CSize</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span><span class='hs-layout'>)</span>
<span class=hs-linenum>869: </span>
<span class=hs-linenum>870: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>memcpy</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>dst</span><span class='hs-conop'>:</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>871: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>src</span><span class='hs-conop'>:</span><span class='hs-conid'>PtrV</span> <span class='hs-conid'>Word8</span> 
<span class=hs-linenum>872: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>size</span><span class='hs-conop'>:</span><span class='hs-keyword'>{CSize | size &lt;= plen src &amp;&amp; size &lt;= plen dst}</span> 
<span class=hs-linenum>873: </span>           <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span> 
<span class=hs-linenum>874: </span>  <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>875: </span><span class='hs-definition'>memcpy</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Ptr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>CSize</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>876: </span><a class=annot href="#"><span class=annottext>x1:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; x2:{VV : (Ptr Word8) | 0 &lt;= plen VV}
-&gt; {size : CSize | size &lt;= plen x2 &amp;&amp; size &lt;= plen x1}
-&gt; (IO ())</span><span class='hs-definition'>memcpy</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | 0 &lt;= plen VV}</span><span class='hs-varid'>q</span></a> <a class=annot href="#"><span class=annottext>{size : CSize | size &lt;= plen q &amp;&amp; size &lt;= plen p}</span><span class='hs-varid'>s</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; x2:{v : (Ptr Word8) | 0 &lt;= plen v}
-&gt; {v : CSize | v &lt;= plen x1 &amp;&amp; v &lt;= plen x2}
-&gt; (IO (Ptr Word8))</span><span class='hs-varid'>c_memcpy</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == p &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>p</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | v == q &amp;&amp; 0 &lt;= plen v}</span><span class='hs-varid'>q</span></a> <a class=annot href="#"><span class=annottext>{v : CSize | v == s &amp;&amp; v &lt;= plen q &amp;&amp; v &lt;= plen p}</span><span class='hs-varid'>s</span></a> <a class=annot href="#"><span class=annottext>(IO (Ptr Word8)) -&gt; (IO ()) -&gt; (IO ())</span><span class='hs-varop'>&gt;&gt;</span></a> <a class=annot href="#"><span class=annottext>() -&gt; (IO ())</span><span class='hs-varid'>return</span></a> <a class=annot href="#"><span class=annottext>{v : () | v == GHC.Tuple.()}</span><span class='hs-conid'>()</span></a>
<span class=hs-linenum>877: </span>
<span class=hs-linenum>878: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>assume</span> <span class='hs-varid'>nullForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyword'>{v:</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span> <span class='hs-keyword'>| fplen v = 0}</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>879: </span><span class='hs-definition'>nullForeignPtr</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>ForeignPtr</span> <span class='hs-conid'>Word8</span>
<span class=hs-linenum>880: </span><a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | fplen v == 0}</span><span class='hs-definition'>nullForeignPtr</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO (ForeignPtr Word8)) -&gt; (ForeignPtr Word8)</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO (ForeignPtr Word8)) -&gt; (ForeignPtr Word8))
-&gt; (IO (ForeignPtr Word8)) -&gt; (ForeignPtr Word8)</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>x1:(Ptr Word8)
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == plen x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>newForeignPtr_</span></a> <a class=annot href="#"><span class=annottext>(Ptr Word8)</span><span class='hs-varid'>nullPtr</span></a>
<span class=hs-linenum>881: </span><span class='hs-comment'>{-# NOINLINE nullForeignPtr #-}</span>
<span class=hs-linenum>882: </span>
<span class=hs-linenum>883: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>create'</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-conid'>Nat</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>PtrN</span> <span class='hs-conid'>Word8</span> <span class='hs-varid'>n</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>ByteStringN</span> <span class='hs-varid'>n</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>884: </span><a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; ({VV : (Ptr Word8) | plen VV == x1 &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ()))
-&gt; {v : ByteString | bLen v == x1}</span><span class='hs-definition'>create'</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt;= 0}</span><span class='hs-varid'>n</span></a> <a class=annot href="#"><span class=annottext>{VV : (Ptr Word8) | plen VV == n &amp;&amp; 0 &lt;= plen VV} -&gt; (IO ())</span><span class='hs-varid'>fill</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(IO {v : ByteString | bLen v == n})
-&gt; {v : ByteString | bLen v == n}</span><span class='hs-varid'>unsafePerformIO</span></a> <a class=annot href="#"><span class=annottext>((IO {v : ByteString | bLen v == n})
 -&gt; {v : ByteString | bLen v == n})
-&gt; (IO {v : ByteString | bLen v == n})
-&gt; {v : ByteString | bLen v == n}</span><span class='hs-varop'>$</span></a> <span class='hs-keyword'>do</span>
<span class=hs-linenum>885: </span>  <a class=annot href="#"><span class=annottext>{VV : (ForeignPtr Word8) | fplen VV == n &amp;&amp; 0 &lt;= fplen VV}</span><span class='hs-varid'>fp</span></a>  <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>x1:{v : Int | v &gt;= 0}
-&gt; (IO {v : (ForeignPtr Word8) | fplen v == x1 &amp;&amp; 0 &lt;= fplen v})</span><span class='hs-varid'>mallocForeignPtrBytes</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a>
<span class=hs-linenum>886: </span>  <a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; ({v : (Ptr Word8) | plen v == fplen x1 &amp;&amp; 0 &lt;= plen v}
    -&gt; (IO ()))
-&gt; (IO ())</span><span class='hs-varid'>withForeignPtr</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; fplen v == n &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : (Ptr Word8) | plen v == n &amp;&amp; 0 &lt;= plen v} -&gt; (IO ())</span><span class='hs-varid'>fill</span></a> 
<span class=hs-linenum>887: </span>  <a class=annot href="#"><span class=annottext>{v : ByteString | bLen v == n}
-&gt; (IO {v : ByteString | bLen v == n})</span><span class='hs-varid'>return</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:(ForeignPtr Word8)
-&gt; x2:{v : Int | v &gt;= 0 &amp;&amp; v &lt;= fplen x1}
-&gt; x3:{v : Int | v &gt;= 0 &amp;&amp; v + x2 &lt;= fplen x1}
-&gt; {v : ByteString | bLen v == x3 &amp;&amp; bOff v == x2 &amp;&amp; bPtr v == x1}</span><span class='hs-conid'>BS</span></a> <a class=annot href="#"><span class=annottext>{v : (ForeignPtr Word8) | v == fp &amp;&amp; fplen v == n &amp;&amp; 0 &lt;= fplen v}</span><span class='hs-varid'>fp</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == (0  :  int)}</span><span class='hs-num'>0</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v == n &amp;&amp; v &gt;= 0}</span><span class='hs-varid'>n</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>888: </span>
</pre>

\end{comment}
