
Case Study: Pointers & Bytes {#case-study-pointers}
===================================================

\begin{comment}
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--prune"          @-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Memory where

import Prelude hiding (null)

import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Data.ByteString.Internal (c2w, w2c)
import Language.Haskell.Liquid.Prelude

spanByte         :: Word8 -> ByteString -> (ByteString, ByteString)
unsafeHead       :: ByteString -> Word8
create, create'  :: Int -> (Ptr Word8 -> IO ()) -> ByteString

-- boilerplate
{-@ type TRUE = {v:Bool | v } @-}

-- TODO: we really shouldn't need this...
-- {- bLen :: b:ByteString -> {v:Nat | v = bLen b} @-}

{-@ type StringN N = {v:String | len v = N} @-}
{-@ type BNat N    = {v:Nat    | v <= N}    @-}
\end{code}
\end{comment}

A large part of the allure of Haskell is its elegant, high-level ADTs
that ensure that programs won't be plagued by problems like the infamous
[SSL heartbleed bug](heartbleed.com).^[Assuming, of course, the absence of errors in the compiler and run-time...]
However, another part of Haskell's charm is that when you really really
need to, you can drop down to low-level pointer twiddling to squeeze the
most performance out of your machine. But of course, that opens the door
to the heartbleeds.

Wouldn't it be nice to have our cake and eat it too?
Wouldn't it be great if we could twiddle pointers at a
low-level and still get the nice safety assurances of
high-level types? Lets see how LiquidHaskell lets us
have our cake and eat it too.


HeartBleeds in Haskell
----------------------

\newthought{Modern Languages} like Haskell are ultimately built upon the
foundation of `C`. Thus, implementation errors could open up unpleasant
vulnerabilities that could easily slither past the type system and even
code inspection. As a concrete example, lets look at a function that
uses the `ByteString` library to truncate strings:

\begin{code}
chop'     :: String -> Int -> String
chop' s n = s'
  where
    b     = pack s          -- down to low-level
    b'    = unsafeTake n b  -- grab n chars
    s'    = unpack b'       -- up to high-level
\end{code}

\noindent First, the function `pack`s the string into a low-level
bytestring `b`, then it grabs the first `n` `Char`acters from `b`
and translates them back into a high-level `String`. Lets see how
the function works on a small test:

~~~~~{.spec}
ghci> let ex = "Ranjit Loves Burritos"
~~~~~

\noindent We get the right result when we `chop` a *valid* prefix:

~~~~~{.spec}
ghci> chop' ex 10
"Ranjit Lov"
~~~~~

\noindent But, as illustrated in Figure [auto](#fig:overflow), the
machine silently reveals (or more colorfully, *bleeds*) the contents
of adjacent memory or if we use an *invalid* prefix:

~~~~~{.spec}
ghci> chop' ex 30
"Ranjit Loves Burritos\NUL\201\&1j\DC3\SOH\NUL"
~~~~~


<div class="figure"
  id="fig:overflow"
  caption="Can we prevent the program from leaking secrets via overflows?"
  height="100px"
  file="img/overflow.png">
</div>


\newthought{Types against Overflows} Now that we have stared the problem
straight in the eye, look at how we can use LiquidHaskell to prevent the
above at compile time. To this end, we decompose the system into
a hierarchy of levels (i.e. modules). Here, we have three levels:

1. *Machine* level `Pointers`
2. *Library* level `ByteString`
3. *User*    level `Application`

\noindent Our strategy, as before, is to develop an *refined API* for
each level such that errors at each level are prevented by using the typed
interfaces for the lower levels. Next, lets see how this strategy lets us
safely manipulate pointers.

Low-level Pointer API
---------------------

To get started, lets look at the low-level pointer API that is
offered by GHC and the run-time. First, lets see who the
*dramatis personae* are and how they might let heartbleeds in.
Then we will see how to batten down the hatches with LiquidHaskell.

\newthought{Pointers} are an (abstract) type `Ptr a` implemented by GHC.

~~~~~{.spec}
-- | A value of type `Ptr a` represents a pointer to an object,
--   or an array of objects, which may be marshalled to or from
--   Haskell values of type `a`.

data Ptr a
~~~~~

\newthought{Foreign Pointers} are *wrapped* pointers that can be
exported to and from C code via the [Foreign Function Interface][foreignptr].

~~~~~{.spec}
data ForeignPtr a
~~~~~

\newthought{To Create} a pointer we use `mallocForeignPtrBytes n`
which creates a `Ptr` to a buffer of size `n` and wraps it as a
`ForeignPtr`

~~~~~{.spec}
mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
~~~~~

\newthought{To Unwrap} and actually use the `ForeignPtr` we use

~~~~~{.spec}
withForeignPtr :: ForeignPtr a     -- pointer
               -> (Ptr a -> IO b)  -- action
               -> IO b             -- result
~~~~~

\noindent That is, `withForeignPtr fp act` lets us execute a
action `act` on the actual `Ptr` wrapped within the `fp`.
These actions are typically sequences of *dereferences*,
i.e. reads or writes.

\newthought{To Dereference} a pointer, i.e. to read or update
the contents at the corresponding memory location, we use
`peek` and `poke` respectively. ^[We elide the `Storable`
type class constraint to strip this presentation down to
the absolute essentials.]

~~~~~{.spec}
peek :: Ptr a -> IO a         -- Read
poke :: Ptr a -> a -> IO ()   -- Write
~~~~~

\newthought{For Fine Grained Access} we can directly shift
pointers to arbitrary offsets using the *pointer arithmetic*
operation `plusPtr p off` which takes a pointer `p` an integer
`off` and returns the address obtained shifting `p` by `off`:

~~~~~{.spec}
plusPtr :: Ptr a -> Int -> Ptr b
~~~~~

\newthought{Example} That was rather dry; lets look at a concrete
example of how one might use the low-level API. The following
function allocates a block of 4 bytes and fills it with zeros:

\begin{code}
zero4 = do fp <- mallocForeignPtrBytes 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 3) zero
           return fp
        where
           zero = 0 :: Word8
\end{code}

\noindent While the above is perfectly all right, a small typo could
easily slip past the type system (and run-time!) leading to hard to find
errors:

\begin{code}
zero4' = do fp <- mallocForeignPtrBytes 4
            withForeignPtr fp $ \p -> do
              poke (p `plusPtr` 0) zero
              poke (p `plusPtr` 1) zero
              poke (p `plusPtr` 2) zero
              poke (p `plusPtr` 8) zero
            return fp
         where
            zero = 0 :: Word8
\end{code}

A Refined Pointer API
---------------------

Wouldn't it be great if we had an assistant to helpfully point out
the error above as soon as we *wrote* it? ^[In Vim or Emacs or online,
you'd see the error helpfully highlighted.] We will use the following
strategy to turn LiquidHaskell into such an assistant:

1. *Refine* pointers with allocated buffer size,
2. *Track* sizes in pointer operations,
3. *Enforce* pointer are valid at reads and writes.

\newthought{To Refine Pointers} with the *size* of their
associated buffers, we can use an *abstract measure*,
i.e. a measure specification *without* any underlying
implementation.

~~~~~{.spec}
-- | Size of `Ptr`
measure plen  :: Ptr a -> Int

-- | Size of `ForeignPtr`
measure fplen :: ForeignPtr a -> Int
~~~~~

\noindent It is helpful to define aliases for pointers of a given size `N`:

~~~~~{.spec}
type PtrN a N        = {v:Ptr a        | plen v  = N}
type ForeignPtrN a N = {v:ForeignPtr a | fplen v = N}
~~~~~

\newthought{Abstract Measures} are extremely useful when we don't have
a concrete implementation of the underlying value, but we know that
the value *exists*.   Here, we don't have the value -- inside Haskell
-- because the buffers are manipulated within C. However, this is no
cause for alarm as we will simply use measures to refine the API, not
to perform any computations. ^[This is another *ghost* specification.]


\newthought{To Refine Allocation} we stipulate that
the size parameter be non-negative, and that the returned
pointer indeed refers to a buffer with exactly `n` bytes:

~~~~~{.spec}
mallocForeignPtrBytes :: n:Nat -> IO (ForeignPtrN a n)
~~~~~

\newthought{To Refine Unwrapping} we specify that the *action*
gets as input, an unwrapped `Ptr` whose size *equals* that of the
given `ForeignPtr`.

~~~~~{.spec}
withForeignPtr :: fp:ForeignPtr a
               -> (PtrN a (fplen fp) -> IO b)
               -> IO b
~~~~~

\noindent This is a rather interesting *higher-order* specification.
Consider a call `withForeignPtr fp act`. If the `act` requires a `Ptr`
whose size *exceeds* that of `fp` then LiquidHaskell will flag a
(subtyping) error indicating the overflow. If instead the `act`
requires a buffer of size less than `fp` then it is always safe
to run the `act` on a larger buffer. For example, the below
variant of `zero4` where we only set the first three bytes
is fine as the `act`, namely the function `\p -> ...`, can be
typed with the requirement that the buffer `p` has size `4`,
even though only `3` bytes are actually touched.

\begin{code}
zero3 = do fp <- mallocForeignPtrBytes 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
           return fp
        where
           zero = 0 :: Word8
\end{code}

\newthought{To Refine Reads and Writes} we specify that they can
only be done if the pointer refers to a non-empty (remaining) buffer.
That is, we define an alias:

~~~~~{.spec}
type OkPtr a = {v:Ptr a | 0 < plen v}
~~~~~

\noindent that describes pointers referring to *non-empty* buffers
(of strictly positive `plen`), and then use the alias to refine:

~~~~~{.spec}
peek :: OkPtr a -> IO a
poke :: OkPtr a -> a -> IO ()
~~~~~

\noindent In essence the above type says that no matter how arithmetic
was used to shift pointers around, when the actual dereference happens,
the size *remaining* after the pointer must be non-negative, so that a
byte can be safely read from or written to the underlying buffer.

\newthought{To Refine the Shift} operations, we simply check that the
pointer *remains* within the bounds of the buffer, and update the `plen`
to reflect the size remaining after the shift: ^[This signature precludes
*left* or *backward* shifts; for that there is an analogous `minusPtr`
which we elide for simplicity.]

~~~~~{.spec}
plusPtr :: p:Ptr a -> off:BNat (plen p) -> PtrN b (plen p - off)
~~~~~

\noindent using the alias `BNat`, defined as:

~~~~~{.spec}
type BNat N = {v:Nat | v <= N}
~~~~~

^[Did you notice that we have strengthened the type of `plusPtr` to
prevent the pointer from wandering outside the boundary of the buffer?
We could instead use a weaker requirement for `plusPtr` that omits
this requirement, and instead have the error be flagged when the
pointer was used to read or write memory.]

\newthought{Types Prevent Overflows} Lets revisit the zero-fill example
from above to understand how the refinements help detect the error:

\begin{code}
exBad = do fp <- mallocForeignPtrBytes 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 5) zero     -- LH complains
           return fp
        where
           zero = 0 :: Word8
\end{code}

\noindent Lets read the tea leaves to understand the above error:

~~~~~{.liquiderror}
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
~~~~~

\noindent The error says we're bumping `p` up by `VV == 5`
using `plusPtr` but the latter *requires* that bump-offset
be within the size of the buffer referred to by `p`, i.e.
`VV <= plen p`. Indeed, in this context, we have:

~~~~~{.liquiderror}
     p    : {p : Ptr a | fplen fp == plen p && ?c <= plen p && ?b <= plen p && zero <= plen p}
     fp   : {fp : ForeignPtr a | fplen fp == ?c && 0 <= fplen fp}
~~~~~

\noindent that is, the size of `p`, namely `plen p` equals the size of
`fp`, namely `fplen fp` (thanks to the `withForeignPtr` call).  The
latter equals to `?c` which is `4` bytes. Thus, since the offset `5`
is not less than the buffer size `4`, LiquidHaskell cannot prove that
the call to `plusPtr` is safe, hence the error.


Assumptions vs Guarantees
-------------------------

At this point you ought to wonder: where is the code for `peek`,
`poke` or `mallocForeignPtrBytes` and so on? How can we be sure
that the types we assigned to them are in fact legitimate?

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
that specification. ^[If we so desire, we can also *check* the boundary
specifications at [run-time](http://en.wikipedia.org/wiki/Design_by_contract),
but that is outside the scope of LiquidHaskell.]

ByteString API
--------------

Next, lets see how the low-level API can be used to implement
to implement [ByteStrings][bytestring], in a way that lets us
perform fast string operations without opening the door to
overflows.


\newthought{A ByteString} is implemented as a record of three fields:

\begin{code}
data ByteString = BS {
    bPtr :: ForeignPtr Word8
  , bOff :: !Int
  , bLen :: !Int
  }
\end{code}

+ `bPtr` is a *pointer* to a block of memory,
+ `bOff` is the *offset* in the block where the string begins,
+ `bLen` is the number of bytes from the offset that belong to the string.

These entities are illustrated in Figure [auto](#fig:bytestring); the
green portion represents the actual contents of a particular
`ByteString`.  This representation makes it possible to implement
various operations like computing prefixes and suffixes extremely
quickly, simply by pointer arithmetic.

<div class="figure"
     id="fig:bytestring"
     height="100px"
     file="img/bytestring.png"
     caption="Representing ByteStrings in memory.">
</div>

\newthought{In a Legal ByteString} the *start* (`bOff`) and *end*
(`bOff + bLen`) offsets lie inside the buffer referred to by the
pointer `bPtr`. We can formalize this invariant with a data definition
that will then make it impossible to create illegal `ByteString`s:

\begin{code}
{-@ data ByteString = BS {
      bPtr :: ForeignPtr Word8
    , bOff :: {v:Nat| v        <= fplen bPtr}
    , bLen :: {v:Nat| v + bOff <= fplen bPtr}
    }
  @-}
\end{code}


\noindent The refinements on `bOff` and `bLen` correspond exactly
to the legality requirements that the start and end of the `ByteString`
be *within* the block of memory referred to by `bPtr`.


\newthought{For brevity} lets define an alias for `ByteString`s of
a given size:

\begin{code}
{-@ type ByteStringN N = {v:ByteString | bLen v = N} @-}
\end{code}

\newthought{Legal Bytestrings}  can be created by directly using
the constructor, as long as we pass in suitable offsets and lengths.
For example,

\begin{code}
{-@ good1 :: IO (ByteStringN 5) @-}
good1 = do fp <- mallocForeignPtrBytes 5
           return (BS fp 0 5)
\end{code}

\noindent creates a valid `ByteString` of size `5`; however we
need not start at the beginning of the block, or use up all
the buffer, and can instead create `ByteString`s whose length
is less than the size of the allocated block, as shown in `good2`
whose length is `2` while the allocated block has size `5`.

\begin{code}
{-@ good2 :: IO (ByteStringN 2) @-}
good2 = do fp <- mallocForeignPtrBytes 5
           return (BS fp 3 2)
\end{code}

\newthought{Illegal Bytestrings} are rejected by LiquidHaskell.
For example, `bad1`'s length is exceeds its buffer
size, and is flagged as such:

\begin{code}
bad1 = do fp <- mallocForeignPtrBytes 3
          return (BS fp 0 10)
\end{code}

\noindent Similarly, `bad2` does have `2` bytes but *not* if
we start at the offset of `2`:

\begin{code}
bad2 = do fp <- mallocForeignPtrBytes 3
          return (BS fp 2 2)
\end{code}

<div class="hwex" id="Legal ByteStrings">
Modify the definitions of `bad1` and `bad2` so they are *accepted* by LiquidHaskell.
</div>

<div class="toolinfo">
\newthought{Measures are generated from Fields} in the datatype
definition.  As GHC lets us use the fields as accessor functions, we
can *refine* the types of those functions to specify their behavior to
LiquidHaskell. For example, we can type the (automatically generated)
field-accessor function `bLen` so that it actually returns the exact
size of the `ByteString` argument.
</div>

\begin{code}
{-@ bLen :: b:ByteString -> {v: Nat | v = bLen b} @-}
\end{code}

\newthought{To Safely Create} a `ByteString` the
implementation defines a higher order `create` function, that
takes a size `n` and accepts a `fill` action, and runs the
action after allocating the pointer. After running the action,
the function tucks the pointer into and returns a `ByteString`
of size `n`.

\begin{code}
{-@ create :: n:Nat -> (Ptr Word8 -> IO ()) -> ByteStringN n @-}
create n fill = unsafePerformIO $ do
  fp  <- mallocForeignPtrBytes n
  withForeignPtr fp fill
  return (BS fp 0 n)
\end{code}

<div class="hwex" id="Create">
\singlestar Why does LiquidHaskell *reject* the following function
that creates a `ByteString` corresponding to `"GHC"`?
</div>

\begin{code}
bsGHC = create 3 $ \p -> do
  poke (p `plusPtr` 0) (c2w 'G')
  poke (p `plusPtr` 1) (c2w 'H')
  poke (p `plusPtr` 2) (c2w 'C')
\end{code}

\hint The function writes into 3 slots starting at `p`.
How big should `plen p` be to allow this? What type
does LiquidHaskell infer for `p` above? Does it meet
the requirement? Which part of the *specification*
or *implementation* needs to be modified so that the
relevant information about `p` becomes available within
the `do`-block above? Make sure you figure out the above
before proceeding.

\newthought{To Pack} a `String` into a `ByteString`
we simply call `create` with the appropriate fill
action:^[The code uses `create'` which is just `create`
with the *correct* signature in case you want to skip
the previous exercise. (But don't!)]

\begin{code}
pack str      = create' n $ \p -> go p xs
  where
  n           = length str
  xs          = map c2w str
  go _ []     = return  ()
  go p (x:xs) = poke p x >> go (p `plusPtr` 1) xs
\end{code}

<div class="hwex" id="Pack">
We can compute the size of a `ByteString` by using the function:
Fix the specification for `pack` so that (it still typechecks!)
and furthermore, the following [QuickCheck style property](#quickcheck)
is proved.
</div>

\begin{code}
{-@ prop_pack_length  :: String -> TRUE @-}
prop_pack_length xs   = bLen (pack xs) == length xs
\end{code}

\hint Look at the type of `length`, and recall that `len`
is a [numeric measure](#numericmeasure) denoting the size
of a list.

\newthought{The magic of inference} ensures that `pack`
just works. Notice there is a tricky little recursive loop
`go` that is used to recursively fill in the `ByteString`
and actually, it has a rather subtle type signature that
LiquidHaskell is able to automatically infer.

<div class="hwex" id="Pack Invariant">
\exercise \singlestar Still, we're here to learn, so can you
*write down* the type signature for the loop so that the below
variant of `pack` is accepted by LiquidHaskell (Do this *without*
cheating by peeping at the type inferred for `go` above!)
</div>

\begin{code}
packEx str     = create' n $ \p -> pLoop p xs
  where
  n            = length str
  xs           = map c2w str

{-@ pLoop      :: (Storable a) => p:Ptr a -> xs:[a] -> IO () @-}
pLoop _ []     = return ()
pLoop p (x:xs) = poke p x >> pLoop (p `plusPtr` 1) xs
\end{code}

\hint Remember that `len xs` denotes the size of the list `xs`.

<div class="hwex" id="Unsafe Take and Drop">
The functions `unsafeTake` and `unsafeDrop` respectively extract
the prefix and suffix of a `ByteString` from a given position.
They are really fast since we only have to change the offsets.
But why does LiquidHaskell reject them? Can you fix the
specifications so that they are accepted?
</div>

\begin{code}
{-@ unsafeTake :: n:Nat -> b:_ -> ByteStringN n @-}
unsafeTake n (BS x s _) = BS x s n

{-@ unsafeDrop :: n:Nat -> b:_ -> ByteStringN {bLen b - n} @-}
unsafeDrop n (BS x s l) = BS x (s + n) (l - n)
\end{code}

\hint Under what conditions are the returned `ByteString`s legal?


\newthought{To Unpack} a `ByteString` into a plain old `String`,
we essentially run `pack` in reverse, by walking over the pointer,
and reading out the characters one by one till we reach the end:

\begin{code}
unpack              :: ByteString -> String
unpack (BS _  _ 0)  = []
unpack (BS ps s l)  = unsafePerformIO
                        $ withForeignPtr ps
                        $ \p -> go (p `plusPtr` s) (l - 1) []
  where
    {-@ go     :: p:_ -> n:_ -> acc:_ -> IO {v:_ | true } @-}
    go p 0 acc = peekAt p 0 >>= \e -> return (w2c e : acc)
    go p n acc = peekAt p n >>= \e -> go p (n-1) (w2c e : acc)
    peekAt p n = peek (p `plusPtr` n)
\end{code}

<div class="hwex" id="Unpack">
\singlestar Fix the specification for `unpack`
so that the below QuickCheck style property is proved by LiquidHaskell.
</div>

\begin{code}
{-@ prop_unpack_length :: ByteString -> TRUE @-}
prop_unpack_length b   = bLen b == length (unpack b)
\end{code}

\hint You will also have to fix the specification of the helper `go`.
Can you determine the output refinement should be (instead of just `true`?)
How *big* is the output list in terms of `p`, `n` and `acc`.


Application API
---------------

Finally, lets revisit our potentially "bleeding" `chop` function to
see how the refined `ByteString` API can prevent errors.  We require
that the prefix size `n` be less than the size of the input string `s`:

\begin{code}
{-@ chop :: s:String -> n:BNat (len s) -> String @-}
chop s n = s'
  where
    b    = pack s          -- down to low-level
    b'   = unsafeTake n b  -- grab n chars
    s'   = unpack b'       -- up to high-level
\end{code}

\newthought{Overflows are prevented} by LiquidHaskell, as it
rejects calls to `chop` where the prefix size is too large
which is what led to the overflow that spilled the contents
of memory after the string (cf. Figure [auto](#fig:overflow)).
In the code below, the first use of `chop` which
defines `ex6` is accepted as `6 <= len ex` but the second
call is rejected as `30 > len ex`.

\begin{code}
demo     = [ex6, ex30]
  where
    ex   = ['L','I','Q','U','I','D']
    ex6  = chop ex 6   -- accepted by LH
    ex30 = chop ex 30  -- rejected by LH
\end{code}

<div class="hwex" id="Chop"> Fix the specification for `chop` so that
the following property is proved:

\begin{code}
{-@ prop_chop_length  :: String -> Nat -> TRUE @-}
prop_chop_length s n
  | n <= length s     = length (chop s n) == n
  | otherwise         = True
\end{code}

<div class="hwex" id="Checked Chop"> In the above, we know statically
that the string is longer than the prefix, but what if the string and prefix
are obtained *dynamically*, e.g. as inputs from the user? Fill in the implementation
of `ok` below to ensure that `chop` is called safely with user specified values:
</div>


\begin{code}
safeChop      :: String -> Int -> String
safeChop str n
  | ok        = chop str n
  | otherwise = ""
  where
    ok        = True

queryAndChop  :: IO String
queryAndChop  = do putStrLn "Give me a string:"
                   str  <-  getLine
                   putStrLn "Give me a number:"
                   ns   <-  getLine
                   let n =  read ns :: Int
                   return $ safeChop str n
\end{code}

Nested ByteStrings
------------------

For a more in-depth example, let's take a look at `group`,
which transforms strings like `"foobaaar"` into *lists* of
strings like `["f","oo", "b", "aaa", "r"]`.
The specification is that `group` should produce a

1. list of *non-empty* `ByteStrings`,
2. the *sum of* whose lengths equals that of the input string.

\newthought{Non-empty ByteStrings} are those whose length is non-zero:

\begin{code}
{-@ predicate Null B  = bLen B == 0                   @-}
{-@ type ByteStringNE = {v:ByteString | not (Null v)} @-}
\end{code}

\noindent We can use these to enrich the API with a `null` check

\begin{code}
{-@ null :: b:_ -> {v:Bool | v <=> Null b} @-}
null (BS _ _ l) = l == 0
\end{code}

\noindent This check is used to determine if it is safe
to extract the head and tail of the `ByteString`.
we can use refinements to ensure the safety of
the operations and also track the sizes. ^[`peekByteOff p i` is equivalent to `peek (plusPtr p i)`.]

\begin{code}
{-@ unsafeHead :: ByteStringNE -> Word8 @-}
unsafeHead (BS x s _) = unsafePerformIO $
                          withForeignPtr x $ \p ->
                            peekByteOff p s

{-@ unsafeTail :: b:ByteStringNE -> ByteStringN {bLen b -1} @-}
unsafeTail (BS ps s l) = BS ps (s + 1) (l - 1)
\end{code}

\newthought{The Group`} function recursively calls `spanByte` to carve off
the next group, and then returns the accumulated results:

\begin{code}
{-@ group :: b:_ -> {v: [ByteStringNE] | bsLen v = bLen b} @-}
group xs
    | null xs   = []
    | otherwise = let  y        = unsafeHead xs
                       (ys, zs) = spanByte y (unsafeTail xs)
                  in (y `cons` ys) : group zs
\end{code}

\noindent The first requirement, that the groups be non-empty is captured by the fact that
the output is a `[ByteStringNE]`. The second requirement, that the sum of the lengths is
preserved, is expressed by a writing a [numeric measure](#numericmeasure):

\begin{code}
{-@ measure bsLen @-}
bsLen        :: [ByteString] -> Int
bsLen []     = 0
bsLen (b:bs) = bLen b + bsLen bs
\end{code}


\newthought{SpanByte} does a lot of the heavy lifting. It uses low-level pointer
arithmetic to find the *first* position in the `ByteString` that is different from
the input character `c` and then splits the `ByteString` into a pair comprising the
prefix and suffix at that point.

\begin{code}
{-@ spanByte :: Word8 -> b:ByteString -> ByteString2 b @-}
spanByte c ps@(BS x s ln)
  = unsafePerformIO
      $ withForeignPtr x $ \p ->
         go (p `plusPtr` s) 0
  where
    go p i
      | i >= ln   = return (ps, empty)
      | otherwise = do c' <- peekByteOff p i
                       if c /= c'
                         then return $ splitAt i
                         else go p (i+1)
    splitAt i     = (unsafeTake i ps, unsafeDrop i ps)
\end{code}

LiquidHaskell infers that `0 <= i <= l` and therefore that
all of the memory accesses are safe. Furthermore, due to
the precise specifications given to `unsafeTake` and
`unsafeDrop`, it is able to prove that the output pair's
lengths add up to the size of the input `ByteString`.

\begin{code}
{-@ type ByteString2 B
      = {v:_ | bLen (fst v) + bLen (snd v) = bLen B} @-}
\end{code}

Recap: Types Against Overflows
------------------------------

In this chapter we saw a case study illustrating how measures
and refinements enable safe low-level pointer arithmetic in
Haskell. The take away messages are that we can:

1. *compose* larger systems from layers of smaller ones,
2. *refine* APIs for each layer, which can be used to
3. *design and validate* the layers above.

We saw this recipe in action by developing a low-level
`Pointer` API, using it to implement fast `ByteString`s
API, and then building some higher-level functions on
top of the `ByteStrings`.

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
\begin{code}
-----------------------------------------------------------------------
-- Helper Code
-----------------------------------------------------------------------

{-@ unsafeCreate :: l:Nat -> (PtrN Word8 l -> IO ()) -> ByteStringN l @-}
unsafeCreate n f = create' n f -- unsafePerformIO $ create n f

{-@ qualif PLLen(v:a, p:b) : len v <= plen p @-}
{-@ qualif ForeignPtrN(v:ForeignPtr a, n:int): fplen v = n @-}
{-@ qualif FPLenPLen(v:Ptr a, fp:ForeignPtr a): fplen fp = plen v @-}
{-@ qualif PtrLen(v:Ptr a, xs:List b): plen v = len xs @-}
{-@ qualif PlenEq(v: Ptr a, x: int): x <= plen v @-}

{-@ cons :: Word8 -> b:ByteString -> {v:ByteStringNE | bLen v = bLen b + 1} @-}
cons :: Word8 -> ByteString -> ByteString
cons c (BS x s l) = unsafeCreate (l+1) $ \p -> withForeignPtr x $ \f -> do
        poke p c
        memcpy (p `plusPtr` 1) (f `plusPtr` s) (fromIntegral l)

{-@ empty :: {v:ByteString | bLen v = 0} @-}
empty :: ByteString
empty = BS nullForeignPtr 0 0

{-@ assume mallocForeignPtrBytes :: n:Nat -> IO (ForeignPtrN a n) @-}
{-@ type ForeignPtrN a N = {v:ForeignPtr a | fplen v = N} @-}
{-@ malloc :: n:Nat -> IO (ForeignPtrN a n) @-}
malloc = mallocForeignPtrBytes

{-@ assume
    c_memcpy :: dst:PtrV Word8
             -> src:PtrV Word8
             -> size:{CSize | size <= plen src && size <= plen dst}
             -> IO (Ptr Word8)
  @-}
foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

{-@ memcpy :: dst:PtrV Word8
           -> src:PtrV Word8
           -> size:{CSize | size <= plen src && size <= plen dst}
           -> IO ()
  @-}
memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memcpy p q s = c_memcpy p q s >> return ()

{-@ assume nullForeignPtr :: {v: ForeignPtr Word8 | fplen v = 0} @-}
nullForeignPtr :: ForeignPtr Word8
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr
{-# NOINLINE nullForeignPtr #-}

{-@ create' :: n:Nat -> (PtrN Word8 n -> IO ()) -> ByteStringN n @-}
create' n fill = unsafePerformIO $ do
  fp  <- mallocForeignPtrBytes n
  withForeignPtr fp fill
  return (BS fp 0 n)
\end{code}
\end{comment}
