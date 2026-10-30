import Data.List
import Data.Nat

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs: List a) -> {rest : _}  -> TakeN (n_xs ++ rest)

takeN : (n: Nat) -> (xs: List a) -> TakeN xs
takeN 0 xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) = case takeN k xs of 
                             Fewer => Fewer
                             Exact ys => Exact ([x] ++ ys)

groupByN : (n : Nat) -> (xs : List a) -> List (List a) 
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs] -- groupByN은 n = 0인 경우 무한 재귀가 발생할 수 있다는 점은 남아 있습니다.
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

halves : List a -> (List a, List a)
halves xs with (takeN (divNatNZ (length xs) 2 ItIsSucc) xs) -- Hint: Use div for dividing a Nat.
  halves xs | Fewer = ([], xs)
  halves (n_xs ++ rest) | (Exact n_xs) = (n_xs, rest)
