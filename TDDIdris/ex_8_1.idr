data ThreeEq : a -> b -> c -> Type where
     Same : (t: a) -> ThreeEq t t t

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

SameS : ThreeEq k j i -> ThreeEq (S k) (S j) (S i)
SameS (Same k) = Same (S k)

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x (Same x) = Same (S x)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> (num3 : Nat) -> Maybe (ThreeEq num1 num2 num3)
checkEqNat 0 0 0 = Just (Same 0)
checkEqNat 0 0 (S k) = Nothing
checkEqNat 0 (S k) 0 = Nothing
checkEqNat 0 (S k) (S j) = Nothing
checkEqNat (S k) 0 0 = Nothing
checkEqNat (S k) 0 (S j) = Nothing
checkEqNat (S k) (S j) 0 = Nothing
checkEqNat (S k) (S j) (S i) = case checkEqNat k j i of
                                    Nothing => Nothing
                                    Just prf => Just (SameS prf)
