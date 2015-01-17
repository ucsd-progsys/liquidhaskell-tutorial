Numeric Measures
================



<pre><span class=hs-linenum> 6: </span>
<span class=hs-linenum> 7: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--diff"</span>           <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 8: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--short-names"</span>    <span class='hs-keyword'>@-}</span>
<span class=hs-linenum> 9: </span><span class='hs-keyword'>{-@</span> <span class='hs-conid'>LIQUID</span> <span class='hs-str'>"--no-termination"</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>10: </span>
<span class=hs-linenum>11: </span><span class='hs-keyword'>module</span> <span class='hs-conid'>NumericMeasures</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>12: </span>
<span class=hs-linenum>13: </span><span class='hs-keyword'>import</span> <span class='hs-keyword'>qualified</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>List</span> <span class='hs-keyword'>as</span> <span class='hs-conid'>L</span>
<span class=hs-linenum>14: </span><span class='hs-keyword'>import</span> <span class='hs-conid'>Prelude</span>                          <span class='hs-varid'>hiding</span>  <span class='hs-layout'>(</span><span class='hs-varid'>zipWith</span><span class='hs-layout'>)</span>
<span class=hs-linenum>15: </span>
</pre>

- 1. Tracking Dimensions "Wholemeal Programming"
      * vector/dotProd
      * matrix/matMult

      * zipWith
      * dotProt
      
-- 2. Defining Numeric Measures
-- 3. Using Numeric Measures

Some of the programs *going wrong* -- and, hence, use cases for LiquidHaskell -- that
we've seen so far, are a consequence of what \cite{Bird} calls *indexitis*
a tendency  to manipulate and hence worry about the low-level details of iterating over an array by keeping track
of a current index. Indeed, functional languages like Haskell encourage
[wholemeal programming]

http://www.cs.ox.ac.uk/ralf.hinze/publications/ICFP09.pdf),
“Functional languages excel at wholemeal programming, a term coined by Geraint Jones. Wholemeal programming means to think big: work with an entire list, rather than a sequence of elements; develop a solution space, rather than an individual solution; imagine a graph, rather than a single path. The wholemeal approach often offers new insights or provides new perspectives on a given problem. It is nicely complemented by the idea of projective programming: first solve a more general problem, then extract the interesting bits and pieces by transforming the general program into more specialised ones.”

Some of the examples 
The examples we've seen so far are rather simple.



<pre><span class=hs-linenum>42: </span><a class=annot href="#"><span class=annottext>forall a b. [a] -&gt; (a -&gt; b) -&gt; [b]</span><span class='hs-definition'>for</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>((a -&gt; b) -&gt; [a] -&gt; [b]) -&gt; [a] -&gt; (a -&gt; b) -&gt; [b]</span><span class='hs-varid'>flip</span></a> <a class=annot href="#"><span class=annottext>(a -&gt; b) -&gt; x3:[a] -&gt; {v : [b] | len v == len x3}</span><span class='hs-varid'>map</span></a>
<span class=hs-linenum>43: </span>
<span class=hs-linenum>44: </span><a class=annot href="#"><span class=annottext>forall a. (Num [Bivariant]
[] a) =&gt; [[a]] -&gt; [[a]] -&gt; [[a]]</span><span class='hs-definition'>mult</span></a> <a class=annot href="#"><span class=annottext>[[a]]</span><span class='hs-varid'>m1</span></a> <a class=annot href="#"><span class=annottext>[[a]]</span><span class='hs-varid'>m2</span></a>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[[a]] -&gt; ([a] -&gt; [a]) -&gt; [[a]]</span><span class='hs-varid'>for</span></a> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == m1 &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>m1</span></a> <a class=annot href="#"><span class=annottext>(([a] -&gt; [a]) -&gt; [[a]]) -&gt; ([a] -&gt; [a]) -&gt; [[a]]</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>ri</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>45: </span>                    <a class=annot href="#"><span class=annottext>[[a]] -&gt; ([a] -&gt; a) -&gt; [a]</span><span class='hs-varid'>for</span></a> <a class=annot href="#"><span class=annottext>{v : [{v : [a] | len v &gt; 0}] | v == m2' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>m2'</span></a> <a class=annot href="#"><span class=annottext>(([a] -&gt; a) -&gt; [a]) -&gt; ([a] -&gt; a) -&gt; [a]</span><span class='hs-varop'>$</span></a> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>cj</span></a> <span class='hs-keyglyph'>-&gt;</span>
<span class=hs-linenum>46: </span>                      <a class=annot href="#"><span class=annottext>[a] -&gt; [a] -&gt; a</span><span class='hs-varid'>dotProd</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == ri &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>ri</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == cj &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>cj</span></a>
<span class=hs-linenum>47: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>48: </span>    <a class=annot href="#"><span class=annottext>[{v : [a] | len v &gt; 0}]</span><span class='hs-varid'>m2'</span></a>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[[a]] -&gt; [{v : [a] | len v &gt; 0}]</span><span class='hs-conid'>L</span></a><span class='hs-varop'>.</span><span class='hs-varid'>transpose</span> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == m2 &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>m2</span></a> 
<span class=hs-linenum>49: </span>    <a class=annot href="#"><span class=annottext>forall a. (Num [Bivariant]
[] a) =&gt; [a] -&gt; [a] -&gt; a</span><span class='hs-varid'>dotProd</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[a] -&gt; a</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>([a] -&gt; a) -&gt; [a] -&gt; a</span><span class='hs-varop'>$</span></a> <a class=annot href="#"><span class=annottext>(a -&gt; a -&gt; a)
-&gt; x4:[a]
-&gt; x5:[a]
-&gt; {v : [a] | len v &lt;= len x5 &amp;&amp; len v &lt;= len x4}</span><span class='hs-conid'>L</span></a><span class='hs-varop'>.</span><span class='hs-varid'>zipWith</span> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:a
-&gt; {VV : a | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; VV &gt;= x1 &amp;&amp; VV &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; VV &gt; x1 &amp;&amp; VV &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; VV == 0}</span><span class='hs-layout'>(</span></a><span class='hs-varop'>*</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == x &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == y &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>y</span></a>
</pre>

[Last time][safeList] we introduced a new specification mechanism called a
*measure* and demonstrated how to use it to encode the *length* of a list.
We saw how measures could be used to verify that functions like `head` and
`tail` were only called with non-empty lists (whose length was strictly
positive). As several folks pointed out, once LiquidHaskell can reason about
lengths, it can do a lot more than just analyze non-emptiness.

Indeed!

Over the next *two* posts, lets see how one might implement a Kmeans
algorithm that clusters `n`-dimensional points groups, and how LiquidHaskell
can help us write and enforce various dimensionality invariants along the way.

Rather than reinvent the wheel, we will modify an existing implementation
of K-Means, [available on hackage][URL-kmeans]. This may not be the
most efficient implementation, but its a nice introduction to the algorithm,
and the general invariants will hold for more sophisticated implementations.

We have broken this entry into two convenient, bite-sized chunks:

+ **Part I**  Introduces the basic types and list operations needed by KMeans,

+ **Part II** Describes how the operations are used in the KMeans implementation.

The Game: Clustering Points
---------------------------

The goal of [K-Means clustering](http://en.wikipedia.org/wiki/K-means_clustering)
is the following. Given

- **Input** : A set of *points* represented by *n-dimensional points*
  in *Euclidian* space, return

- **Output** : A partitioning of the points, into K clusters, in a manner that
  minimizes sum of distances between each point and its cluster center.


The Players: Types
------------------

Lets make matters concrete by creating types for the different elements of the algorithm.

**1. Fixed-Length Lists**  We will represent n-dimensional points using
good old Haskell lists, refined with a predicate that describes the
dimensionality (i.e. length.) To simplify matters, lets package this
into a *type alias* that denotes lists of a given length `N`.


<pre><span class=hs-linenum>100: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span> <span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>N</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

**2. Points** Next, we can represent an `N`-dimensional point as list of `Double` of length `N`,


<pre><span class=hs-linenum>106: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Point</span> <span class='hs-conid'>N</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>List</span> <span class='hs-conid'>Double</span> <span class='hs-conid'>N</span> <span class='hs-keyword'>@-}</span>
</pre>

**3. Clusters** A cluster is a **non-empty** list of points,


<pre><span class=hs-linenum>112: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>NonEmptyList</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span> <span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-varop'>&gt;</span> <span class='hs-num'>0</span><span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

**4. Clustering** And finally, a clustering is a list of (non-empty) clusters.


<pre><span class=hs-linenum>118: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Clustering</span> <span class='hs-varid'>a</span>  <span class='hs-keyglyph'>=</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>NonEmptyList</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyword'>@-}</span>
</pre>

**Notation:** When defining refinement type aliases, we use uppercase variables like `N`
to distinguish value- parameters from the lowercase type parameters like `a`.


**Aside:** By the way, if you are familiar with the *index-style* length
encoding e.g. as found in [DML][dml] or [Agda][agdavec], then its worth
noting that despite appearances, our `List` and `Point` definitions are
*not* indexed. We're just using the indices to define abbreviations for the
refinement predicates, and we have deliberately chosen the predicates to
facilitate SMT based checking and inference.

Basic Operations on Points and Clusters
=======================================

Ok, with the types firmly in hand, let us go forth and develop the KMeans
clustering implementation. We will use a variety of small helper functions
(of the kind found in `Data.List`.) Lets get started by looking at them
through our newly *refined* eyes.

Grouping
--------

The first such function is [groupBy][URL-groupBy]. We can
refine its type so that instead of just producing a `[[a]]`
we know that it produces a `Clustering a` which is a list
of *non-empty* lists.


<pre><span class=hs-linenum>149: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>groupBy</span>       <span class='hs-keyglyph'>::</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Clustering</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>150: </span><a class=annot href="#"><span class=annottext>forall a. (a -&gt; a -&gt; Bool) -&gt; [a] -&gt; [{v : [a] | len v &gt; 0}]</span><span class='hs-definition'>groupBy</span></a> <span class='hs-keyword'>_</span>  <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x4 VV -&gt; p x4&gt; | null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>151: </span><span class='hs-definition'>groupBy</span> <span class='hs-varid'>eq</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | null v &lt;=&gt; false &amp;&amp; len v == 1 + len x2 &amp;&amp; xListSelector v == x1 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a><a class=annot href="#"><span class=annottext>{v : [a] | v == ys &amp;&amp; v == ys &amp;&amp; len v == len ys &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len xs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:{v : [a] | len v &gt; 0}
-&gt; x2:[{v : [a] | len v &gt; 0}]&lt;\_ VV -&gt; len v &gt; 0&gt;
-&gt; {v : [{v : [a] | len v &gt; 0}]&lt;\_ VV -&gt; len v &gt; 0&gt; | null v &lt;=&gt; false &amp;&amp; len v == 1 + len x2 &amp;&amp; xListSelector v == x1 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a. (a -&gt; a -&gt; Bool) -&gt; [a] -&gt; [{v : [a] | len v &gt; 0}]</span><span class='hs-varid'>groupBy</span></a> <a class=annot href="#"><span class=annottext>a -&gt; a -&gt; Bool</span><span class='hs-varid'>eq</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; v == zs &amp;&amp; len v == len zs &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len xs}</span><span class='hs-varid'>zs</span></a>
<span class=hs-linenum>152: </span>  <span class='hs-keyword'>where</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == ys &amp;&amp; len VV == len ys &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len xs}</span><span class='hs-varid'>ys</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>{VV : [a] | VV == zs &amp;&amp; len VV == len zs &amp;&amp; len VV &gt;= 0 &amp;&amp; len VV &lt;= len xs}</span><span class='hs-varid'>zs</span></a><span class='hs-layout'>)</span>   <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(a -&gt; Bool)
-&gt; x3:[a]
-&gt; ({v : [a] | len v &lt;= len x3}, {v : [a] | len v &lt;= len x3})</span><span class='hs-varid'>span</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>a -&gt; a -&gt; Bool</span><span class='hs-varid'>eq</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>

Intuitively, its pretty easy to see how LiquidHaskell verifies the refined
specification:

- Each element of the output list is of the form `x:ys`
- For any list `ys` the length is non-negative, i.e. `(len ys) >= 0`
- The `len` of `x:ys` is `1 + (len ys)`, that is, strictly positive.

Partitioning
------------

Next, lets look the function


<pre><span class=hs-linenum>168: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>partition</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>size</span><span class='hs-conop'>:</span><span class='hs-conid'>PosInt</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Clustering</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>169: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>PosInt</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&gt;</span> <span class='hs-num'>0</span> <span class='hs-layout'>}</span> <span class='hs-keyword'>@-}</span>
</pre>

which is given a *strictly positive* integer argument,
a list of `a` values, and returns a `Clustering a`,
that is, a list of non-empty lists. (Each inner list has a length
that is less than `size`, but we shall elide this for simplicity.)

The function is implemented in a straightforward manner, using the
library functions `take` and `drop`


<pre><span class=hs-linenum>181: </span><a class=annot href="#"><span class=annottext>forall a. {v : Int | v &gt; 0} -&gt; [a] -&gt; [{v : [a] | len v &gt; 0}]</span><span class='hs-definition'>partition</span></a> <span class='hs-keyword'>_</span>    <span class='hs-conid'>[]</span>       <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x4 VV -&gt; p x4&gt; | null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>182: </span><span class='hs-definition'>partition</span> <span class='hs-varid'>size</span> <span class='hs-varid'>ys</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-keyword'>_</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs</span></a> <a class=annot href="#"><span class=annottext>x1:{v : [a] | len v &gt; 0}
-&gt; x2:[{v : [a] | len v &gt; 0}]&lt;\_ VV -&gt; len v &gt; 0&gt;
-&gt; {v : [{v : [a] | len v &gt; 0}]&lt;\_ VV -&gt; len v &gt; 0&gt; | null v &lt;=&gt; false &amp;&amp; len v == 1 + len x2 &amp;&amp; xListSelector v == x1 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a. {v : Int | v &gt; 0} -&gt; [a] -&gt; [{v : [a] | len v &gt; 0}]</span><span class='hs-varid'>partition</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt; 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == zs' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>zs'</span></a>
<span class=hs-linenum>183: </span>  <span class='hs-keyword'>where</span>
<span class=hs-linenum>184: </span>    <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>zs</span></a>                  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:[a]
-&gt; {v : [a] | not (x1 &gt;= 0) =&gt; len v == 0 &amp;&amp; x1 &gt;= 0 =&gt; len v == if (len x2 &lt; x1) then len x2 else x1}</span><span class='hs-varid'>take</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt; 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
<span class=hs-linenum>185: </span>    <a class=annot href="#"><span class=annottext>[a]</span><span class='hs-varid'>zs'</span></a>                 <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:Int
-&gt; x2:[a]
-&gt; {v : [a] | not (x1 &gt;= 0) =&gt; len v == len x2 &amp;&amp; x1 &gt;= 0 =&gt; len v == if (len x2 &lt; x1) then 0 else len x2 - x1}</span><span class='hs-varid'>drop</span></a> <a class=annot href="#"><span class=annottext>{v : Int | v &gt; 0}</span><span class='hs-varid'>size</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v &gt;= 0}</span><span class='hs-varid'>ys</span></a>
</pre>

To verify that a valid `Clustering` is produced, LiquidHaskell needs only
verify that the list `zs` above is non-empty, by suitably connecting the
properties of the inputs `size` and `ys` with the output.

We have [verified elsewhere][URL-take] that


<pre><span class=hs-linenum>195: </span><span class='hs-definition'>take</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>n</span><span class='hs-conop'>:</span><span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>|</span> <span class='hs-varid'>v</span> <span class='hs-varop'>&gt;=</span> <span class='hs-num'>0</span> <span class='hs-layout'>}</span>
<span class=hs-linenum>196: </span>     <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>197: </span>     <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span><span class='hs-varid'>v</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-keyword'>if</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-varop'>&lt;</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-keyword'>then</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyword'>else</span> <span class='hs-varid'>n</span><span class='hs-layout'>)</span> <span class='hs-layout'>}</span>
</pre>

In other words, the output list's length is the *smaller of* the input
list's length and `n`.  Thus, since both `size` and the `(len ys)` are
greater than `1`, LiquidHaskell deduces that the list returned by `take
size ys` has a length greater than `1`, i.e., is non-empty.

Zipping
-------

To compute the *Euclidean distance* between two points, we will use
the `zipWith` function. We must make sure that it is invoked on points
with the same number of dimensions, so we write


<pre><span class=hs-linenum>213: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>zipWith</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span>
<span class=hs-linenum>214: </span>            <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>b</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>c</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
<span class=hs-linenum>215: </span><a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | len v == len x4}
-&gt; {v : [c] | len v == len x4}</span><span class='hs-definition'>zipWith</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-conop'>:</span><span class='hs-keyword'>as</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>b</span><span class='hs-conop'>:</span><span class='hs-varid'>bs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == a}</span><span class='hs-varid'>a</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == b}</span><span class='hs-varid'>b</span></a> <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>forall a b c.
(a -&gt; b -&gt; c)
-&gt; x4:[a]
-&gt; {v : [b] | len v == len x4}
-&gt; {v : [c] | len v == len x4}</span><span class='hs-varid'>zipWith</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == as &amp;&amp; len v &gt;= 0}</span><span class='hs-keyword'>as</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == bs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>bs</span></a>
<span class=hs-linenum>216: </span><span class='hs-definition'>zipWith</span> <span class='hs-keyword'>_</span> <span class='hs-conid'>[]</span> <span class='hs-conid'>[]</span>         <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x4 VV -&gt; p x4&gt; | null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
</pre>

The type stipulates that the second input list and the output have
the same length as the first input. Furthermore, it rules out the
case where one list is empty and the other is not, as in that case
the former's length is zero while the latter's is not.

Transposing
-----------

The last basic operation that we will require is a means to
*transpose* a `Matrix`, which itself is just a list of lists:


<pre><span class=hs-linenum>231: </span><span class='hs-keyword'>{-@</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-conid'>Rows</span> <span class='hs-conid'>Cols</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-conid'>Cols</span><span class='hs-layout'>)</span> <span class='hs-conid'>Rows</span><span class='hs-layout'>)</span> <span class='hs-keyword'>@-}</span>
</pre>

The `transpose` operation flips the rows and columns. I confess that I
can never really understand matrices without concrete examples,
and even then, barely.

So, lets say we have a *matrix*


<pre><span class=hs-linenum>241: </span><span class='hs-definition'>m1</span>  <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-conid'>Int</span> <span class='hs-num'>4</span> <span class='hs-num'>2</span>
<span class=hs-linenum>242: </span><span class='hs-definition'>m1</span>  <span class='hs-keyglyph'>=</span>  <span class='hs-keyglyph'>[</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>1</span><span class='hs-layout'>,</span> <span class='hs-num'>2</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>243: </span>       <span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>3</span><span class='hs-layout'>,</span> <span class='hs-num'>4</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>244: </span>       <span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>5</span><span class='hs-layout'>,</span> <span class='hs-num'>6</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>245: </span>       <span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>7</span><span class='hs-layout'>,</span> <span class='hs-num'>8</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>]</span>
</pre>

then the matrix `m2 = transpose 2 3 m1` should be


<pre><span class=hs-linenum>251: </span><span class='hs-definition'>m2</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-conid'>Int</span> <span class='hs-num'>2</span> <span class='hs-num'>4</span>
<span class=hs-linenum>252: </span><span class='hs-definition'>m2</span>  <span class='hs-keyglyph'>=</span>  <span class='hs-keyglyph'>[</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>1</span><span class='hs-layout'>,</span> <span class='hs-num'>3</span><span class='hs-layout'>,</span> <span class='hs-num'>5</span><span class='hs-layout'>,</span> <span class='hs-num'>7</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>253: </span>       <span class='hs-layout'>,</span> <span class='hs-keyglyph'>[</span><span class='hs-num'>2</span><span class='hs-layout'>,</span> <span class='hs-num'>4</span><span class='hs-layout'>,</span> <span class='hs-num'>6</span><span class='hs-layout'>,</span> <span class='hs-num'>8</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>]</span>
</pre>

We will use a `Matrix a m n` to represent a *single cluster* of `m` points
each of which has `n` dimensions. We will transpose the matrix to make it
easy to *sum* and *average* the points along *each* dimension, in order to
compute the *center* of the cluster.

As you can work out from the above, the code for `transpose` is quite
straightforward: each *output row* is simply the list of `head`s of
the *input rows*:


<pre><span class=hs-linenum>266: </span><span class='hs-definition'>transpose</span>       <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>267: </span><a class=annot href="#"><span class=annottext>forall a.
x1:Int
-&gt; x2:{v : Int | v &gt; 0}
-&gt; {v : [{v : [a] | len v == x1}] | len v == x2}
-&gt; {v : [{v : [a] | len v == x2}] | len v == x1}</span><span class='hs-definition'>transpose</span></a> <span class='hs-num'>0</span> <span class='hs-keyword'>_</span> <span class='hs-keyword'>_</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a &lt;p :: a a -&gt; Prop&gt;.
{v : [a]&lt;\x4 VV -&gt; p x4&gt; | null v &lt;=&gt; true &amp;&amp; len v == 0}</span><span class='hs-conid'>[]</span></a>
<span class=hs-linenum>268: </span><span class='hs-definition'>transpose</span> <span class='hs-varid'>c</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>col00</span> <span class='hs-conop'>:</span> <span class='hs-varid'>col01s</span><span class='hs-layout'>)</span> <span class='hs-conop'>:</span> <span class='hs-varid'>row1s</span><span class='hs-layout'>)</span>
<span class=hs-linenum>269: </span>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | null v &lt;=&gt; false &amp;&amp; v == row0' &amp;&amp; xListSelector v == col00 &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>row0'</span></a> <a class=annot href="#"><span class=annottext>x1:{v : [a] | len v == len rest &amp;&amp; len v == len fix &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len row1s}
-&gt; x2:[{v : [a] | len v == len x1 &amp;&amp; len v == len rest &amp;&amp; len v == len fix &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len row1s}]&lt;\x21 VV -&gt; len v == len x21 &amp;&amp; len v == len rest &amp;&amp; len v == len fix &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len row1s&gt;
-&gt; {v : [{v : [a] | len v == len rest &amp;&amp; len v == len fix &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len row1s}]&lt;\x9 VV -&gt; len v == len rest &amp;&amp; len v == len x9 &amp;&amp; len v == len fix &amp;&amp; len v &gt; 0 &amp;&amp; len v &gt; len row1s&gt; | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == row1s' &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>row1s'</span></a>
<span class=hs-linenum>270: </span>    <span class='hs-keyword'>where</span>
<span class=hs-linenum>271: </span>      <a class=annot href="#"><span class=annottext>{v : [a] | null v &lt;=&gt; false &amp;&amp; xListSelector v == col00}</span><span class='hs-varid'>row0'</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == col00}</span><span class='hs-varid'>col00</span></a>  <a class=annot href="#"><span class=annottext>x1:a
-&gt; x2:[a]
-&gt; {v : [a] | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | len v == len row1s &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt; len rest &amp;&amp; len v &lt;= len row1s}</span><span class='hs-keyglyph'>[</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == col0}</span><span class='hs-varid'>col0</span></a>  <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>col0</span> <span class='hs-conop'>:</span> <span class='hs-keyword'>_</span><span class='hs-layout'>)</span>  <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == row1s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>row1s</span></a> <span class='hs-keyglyph'>]</span>
<span class=hs-linenum>272: </span>      <a class=annot href="#"><span class=annottext>{v : [{v : [a] | len v == len col01s &amp;&amp; len v &gt;= 0}]&lt;\x7 VV -&gt; len v == len col01s &amp;&amp; len v == len x7 &amp;&amp; len v &gt;= 0&gt; | null v &lt;=&gt; false &amp;&amp; xListSelector v == col01s}</span><span class='hs-varid'>rest</span></a>      <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : [a] | v == col01s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>col01s</span></a> <a class=annot href="#"><span class=annottext>x1:{v : [a] | len v == len col01s &amp;&amp; len v &gt;= 0}
-&gt; x2:[{v : [a] | len v == len col01s &amp;&amp; len v == len x1 &amp;&amp; len v &gt;= 0}]&lt;\x18 VV -&gt; len v == len col01s &amp;&amp; len v == len x18 &amp;&amp; len v &gt;= 0&gt;
-&gt; {v : [{v : [a] | len v == len col01s &amp;&amp; len v &gt;= 0}]&lt;\x9 VV -&gt; len v == len col01s &amp;&amp; len v == len x9 &amp;&amp; len v &gt;= 0&gt; | null v &lt;=&gt; false &amp;&amp; xListSelector v == x1 &amp;&amp; len v == 1 + len x2 &amp;&amp; xsListSelector v == x2}</span><span class='hs-conop'>:</span></a> <a class=annot href="#"><span class=annottext>{v : [{v : [a] | len v == len col01s &amp;&amp; len v &gt;= 0}] | len v == len row1s &amp;&amp; len v &gt;= 0 &amp;&amp; len v &lt;= len row1s}</span><span class='hs-keyglyph'>[</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == col1s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>col1s</span></a> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span> <span class='hs-conop'>:</span> <span class='hs-varid'>col1s</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <a class=annot href="#"><span class=annottext>{v : [[a]] | v == row1s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>row1s</span></a> <span class='hs-keyglyph'>]</span>
<span class=hs-linenum>273: </span>      <a class=annot href="#"><span class=annottext>[[a]]</span><span class='hs-varid'>row1s'</span></a>    <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>forall a.
x1:Int
-&gt; x2:{v : Int | v &gt; 0}
-&gt; {v : [{v : [a] | len v == x1}] | len v == x2}
-&gt; {v : [{v : [a] | len v == x2}] | len v == x1}</span><span class='hs-varid'>transpose</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>Int</span><span class='hs-varid'>c</span></a><a class=annot href="#"><span class=annottext>x1:Int -&gt; x2:Int -&gt; {v : Int | v == x1 - x2}</span><span class='hs-comment'>-</span></a><a class=annot href="#"><span class=annottext>{v : Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : Int | v &gt; 0}</span><span class='hs-varid'>r</span></a> <a class=annot href="#"><span class=annottext>{v : [{v : [a] | len v == len col01s &amp;&amp; len v &gt;= 0}]&lt;\x9 VV -&gt; len v == len col01s &amp;&amp; len v == len x9 &amp;&amp; len v &gt;= 0&gt; | null v &lt;=&gt; false &amp;&amp; v == rest &amp;&amp; xListSelector v == col01s &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>rest</span></a>
</pre>

LiquidHaskell verifies that


<pre><span class=hs-linenum>279: </span><span class='hs-keyword'>{-@</span> <span class='hs-varid'>transpose</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>c</span><span class='hs-conop'>:</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>r</span><span class='hs-conop'>:</span><span class='hs-conid'>PosInt</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-varid'>r</span> <span class='hs-varid'>c</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-varid'>c</span> <span class='hs-varid'>r</span> <span class='hs-keyword'>@-}</span>
</pre>

Try to work it out for yourself on pencil and paper.

If you like you can get a hint by seeing how LiquidHaskell figures it out.
Lets work *backwards*.

LiquidHaskell verifies the output type by inferring that


<pre><span class=hs-linenum>290: </span><span class='hs-definition'>row0'</span>        <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span>
<span class=hs-linenum>291: </span><span class='hs-definition'>row1s'</span>       <span class='hs-keyglyph'>::</span> <span class='hs-conid'>List</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span> <span class='hs-comment'>-</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-comment'>-- i.e. Matrix a (c - 1) r</span>
</pre>

and so, by simply using the *measure-refined* type for `:`


<pre><span class=hs-linenum>297: </span><span class='hs-layout'>(</span><span class='hs-conop'>:</span><span class='hs-layout'>)</span>          <span class='hs-keyglyph'>::</span> <span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>xs</span><span class='hs-conop'>:</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>{</span> <span class='hs-varid'>v</span> <span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>1</span> <span class='hs-varop'>+</span> <span class='hs-layout'>(</span><span class='hs-varid'>len</span> <span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-layout'>}</span>
</pre>

LiquidHaskell deduces that


<pre><span class=hs-linenum>303: </span><span class='hs-definition'>row0</span> <span class='hs-conop'>:</span> <span class='hs-varid'>rows'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>List</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-varid'>r</span><span class='hs-layout'>)</span> <span class='hs-varid'>c</span>
</pre>

That is,


<pre><span class=hs-linenum>309: </span><span class='hs-definition'>row0</span> <span class='hs-conop'>:</span> <span class='hs-varid'>rows'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-varid'>c</span> <span class='hs-varid'>r</span>
</pre>

Excellent! Now, lets work backwards. How does it infer the types of `row0'` and `row1s'`?

The first case is easy: `row0'` is just the list of *heads* of each row, hence a `List a r`.

Now, lets look at `row1s'`. Notice that `row1s` is the matrix of all *except* the first row of the input Matrix, and so


<pre><span class=hs-linenum>319: </span><span class='hs-definition'>row1s</span>        <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>r</span><span class='hs-comment'>-</span><span class='hs-num'>1</span><span class='hs-layout'>)</span> <span class='hs-varid'>c</span>
</pre>

and so, as


<pre><span class=hs-linenum>325: </span><span class='hs-definition'>col01s</span>       <span class='hs-keyglyph'>::</span> <span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-comment'>-</span><span class='hs-num'>1</span><span class='hs-layout'>)</span>
<span class=hs-linenum>326: </span><span class='hs-definition'>col1s</span>        <span class='hs-keyglyph'>::</span> <span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-comment'>-</span><span class='hs-num'>1</span><span class='hs-layout'>)</span>
</pre>

LiquidHaskell deduces that since `rest` is the concatenation of `r-1` tails from `row1s`


<pre><span class=hs-linenum>332: </span><span class='hs-definition'>rest</span>         <span class='hs-keyglyph'>=</span> <span class='hs-varid'>col01s</span> <span class='hs-conop'>:</span> <span class='hs-keyglyph'>[</span> <span class='hs-varid'>col1s</span> <span class='hs-keyglyph'>|</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span> <span class='hs-conop'>:</span> <span class='hs-varid'>col1s</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>row1s</span> <span class='hs-keyglyph'>]</span>
</pre>

the type of `rest` is


<pre><span class=hs-linenum>338: </span><span class='hs-definition'>rest</span>         <span class='hs-keyglyph'>::</span> <span class='hs-conid'>List</span> <span class='hs-layout'>(</span><span class='hs-conid'>List</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span> <span class='hs-comment'>-</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-varid'>r</span>
</pre>

which is just


<pre><span class=hs-linenum>344: </span><span class='hs-definition'>rest</span>         <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Matrix</span> <span class='hs-varid'>a</span> <span class='hs-varid'>r</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-comment'>-</span><span class='hs-num'>1</span><span class='hs-layout'>)</span>
</pre>

Now, LiquidHaskell deduces `row1s' :: Matrix a (c-1) r` by inductively
plugging in the output type of the recursive call, thereby checking the
function's signature.

*Whew!* That was a fair bit of work, wasn't it!

Happily, we didn't have to do *any* of it. Instead, using the SMT solver,
LiquidHaskell ploughs through calculations like that and guarantees to us
that `transpose` indeed flips the dimensions of the inner and outer lists.

**Aside: Comprehensions vs. Map** Incidentally, the code above is essentially
that of `transpose` [from the Prelude][URL-transpose] with some extra
local variables for exposition. You could instead use a `map head` and `map tail`
and I encourage you to go ahead and [see for yourself.][demo]

Intermission
------------

Time for a break -- [go see a cat video!][maru] -- or skip it, stretch your
legs, and return post-haste for the [next installment][kmeansII], in which
we will use the types and functions described above, to develop the clustering
algorithm.

[safeList]:      /blog/2013/01/31/safely-catching-a-list-by-its-tail.lhs/
[kmeansI]:       /blog/2013/02/16/kmeans-clustering-I.lhs/
[kmeansII]:      /blog/2013/02/17/kmeans-clustering-II.lhs/
[URL-take]:      https://github.com/ucsd-progsys/liquidhaskell/blob/master/include/GHC/List.lhs#L334
[URL-groupBy]:   http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:groupBy
[URL-transpose]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-List.html#transpose
[maru]:          http://www.youtube.com/watch?v=8uDuls5TyNE
[demo]:          http://goto.ucsd.edu/~rjhala/liquid/haskell/demo/#?demo=KMeansHelper.hs
[URL-kmeans]:    http://hackage.haskell.org/package/kmeans
[dml]:           http://www.cs.bu.edu/~hwxi/DML/DML.html
[agdavec]:       http://code.haskell.org/Agda/examples/Vec.agda

