-- import Data.Vect
import Decidable.Equality
data Vect : (n : Nat) -> (a: Type) -> Type where
     Nil : Vect 0 a
     (::) : a -> Vect n a -> Vect (S n) a

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
       (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
       (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
   decEq [] [] = Yes Refl
   decEq (x :: z) (y :: w) = case decEq x y of 
                                  Yes headPrf => case decEq z w of
                                             Yes Refl => Yes (cong (\v => v :: z) headPrf)
                                             No acontra => No (tailUnequal acontra)
                                  No contra => No (headUnequal contra)
