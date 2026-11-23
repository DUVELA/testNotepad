import Data.Primitives.Views
import InfList
{- 
ever_other : Stream Nat -> Stream Nat
ever_other (x :: y) = 2 * x :: ever_other y
-}
every_other : Stream a -> Stream a
every_other (x :: y :: ys) = y :: every_other ys 
