import Data.Vect

myReverse : Vect n a -> Vect n a
myReverse [] = []
myReverse {n = S k} (x :: xs) = let result = myReverse xs ++ [x] in
                                    rewrite plusCommutative 1 k in result

addS : (j: Nat) -> (k: Nat) -> S (j + k) = j + S k
addS 0 0 = Refl
addS 0 (S k) = Refl
addS (S j) 0 = cong S (addS j 0)
addS (S j) (S k) = cong S (addS j (S k))

{-
Main> :t plusCommutative
Data.Nat.plusCommutative : (left : Nat) -> (right : Nat) -> left + right = right + left
 -}
ddt : (l : Nat) -> (r: Nat) -> l + r = r + l
ddt 0 0 = Refl
ddt 0 (S k) = cong S (sym (ddt k 0))
ddt (S k) 0 = cong S (ddt k 0)
ddt (S k) (S j) = cong S (trans (ddt k (S j)) (addS j k)) -- Builtin.trans : (0 _ : a = b) -> (0 _ : b = c) -> a = c
