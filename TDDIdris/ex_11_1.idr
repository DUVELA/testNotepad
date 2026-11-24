import Data.Primitives.Views
import InfList
import Randoms

data Face = Heads | Tails

{- 
ever_other : Stream Nat -> Stream Nat
ever_other (x :: y) = 2 * x :: ever_other y
-}
every_other : Stream a -> Stream a
every_other (x :: y :: ys) = y :: every_other ys 


{- 내가 한 방식 getFace i = if mod i 2 == 0 then Heads else Tails -}
getFace : Int -> Face
getFace i with (divides i 2)
  getFace ((2 * div) + rem) | (DivBy div rem prf) = if rem == 0 then Heads else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips 0 _ = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs

{
