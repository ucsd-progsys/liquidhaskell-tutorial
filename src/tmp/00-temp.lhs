I am a File
===========

\begin{comment}
\begin{code}
{-@ LIQUID "--short-names" @-}
module Logic where
main :: IO ()
main = return ()


{-@ type TRUE  = {v:Bool | Prop v} @-}
{-@ type FALSE = {v:Bool | not (Prop v)} @-}


{-@ (==>) :: p:Bool -> q:Bool -> {v:Bool | Prop v <=> (Prop p =>  Prop q)} @-}
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

{-@ measure f :: Int -> Int @-}
{-@ f :: x:Int -> {v:Int | v = f x} @-}
f :: Int -> Int
f = undefined
\end{code}
\end{comment}




This is Section 1
-----------------

Yabba dabba doo now.

\begin{code}
{-@ incr :: Nat -> Nat @-}
incr :: Int -> Int
incr x = x + 1
\end{code}

And now, Section 2
------------------

Gibbity dibbity doo now.

\begin{code}
{-@ wincr :: Nat -> Nat @-}
wincr :: Int -> Int
wincr x = x - 1
\end{code}

subtyping
---------

this is the end


\begin{code}
{-@ congruence :: Int -> Int -> TRUE @-}
congruence x y = (x == y) ==> (f x == f y)
\end{code}

