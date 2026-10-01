import Data.Vect

addS : (j: Nat) -> (k: Nat) -> S (j + k) = j + S k
addS 0 0 = Refl
addS 0 (S k) = Refl
addS (S j) 0 = cong S (addS j 0)
addS (S j) (S k) = cong S (addS j (S k))

ddt : (l : Nat) -> (r: Nat) -> l + r = r + l
ddt 0 0 = Refl
ddt 0 (S k) = cong S (sym (ddt k 0))
ddt (S k) 0 = cong S (ddt k 0)
ddt (S k) (S j) = cong S (trans (ddt k (S j)) (addS j k))

myplusCommutest : (n: Nat) -> (m: Nat) -> n + m = m + n
myplusCommutest 0 m = sym (plusZeroRightNeutral m)
myplusCommutest (S k) m = 
   rewrite myplusCommutest k m in 
   rewrite plusSuccRightSucc m k in 
   Refl

reverse_rhs0 : Vect (S (plus k len)) a -> Vect (plus k (S len)) a
reverse_rhs0 xs = rewrite sym (plusSuccRightSucc k len) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect k a -> Vect m a -> Vect (k + m) a
        reverse' acc [] = rewrite plusZeroRightNeutral k in acc
        reverse' acc (x :: xs) = reverse_rhs0 (reverse' (x :: acc) xs)
