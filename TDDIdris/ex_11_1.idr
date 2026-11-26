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


square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number next
   where
      next : Double
      next = (approx + (number / approx)) / 2

square_root_bound 0 number bound (approx :: approxs) = approx
square_root_bound (S k) number bound (approx :: approxs) = 
   if abs (approx * approx - number) > bound then -- bound로 근사값을 사용하지 않았다고 해서 추가
      square_root_bound k number bound approxs
   else approx

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                 (square_root_approx number number)

{-
square_root_approx 감을 못 잡아서.감도 그렇지만
view를 쓴다면? 재귀를 돌기 더 편하지 않을까? 그런데 적절한 건? <- 모름.
그럼 next 공식을 그대로 쓰나? <- 어떻게?
재귀로 next가 들어간다면 ?? :: square_root_approx number next 이건가? <- 그렇겠지? number 위치가 바뀐다면 기준이 사라지는 걸테니깐?
next 식을 그냥 쓰면 불편하게 느껴진다 그럼 어떻게? let이나 where를 쓰자.
그걸 어떻게?
Arith.idr에 where 방식이 있다.

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
   where
      bound : Int -> Int
      bound num with (divides num 12)
        bound ((12 * div) + rem) | (DivBy div rem prf) = rem + 1

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = next :: square_root_approx number next
   where
      next : Double
      next = (approx + (number / approx)) / 2
-}
