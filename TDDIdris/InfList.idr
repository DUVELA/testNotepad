-- chapter 11
data InfList : Type -> Type where
     (::) : (value : a) -> Inf (InfList a) -> InfList a
%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count: Nat) -> InfList a -> List a
getPrefix 0 xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k (Force xs)
