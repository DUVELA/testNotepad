import Data.Vect

zipWithIndexHelp : Nat -> Vect n a -> Vect n (a, Nat)
zipWithIndexHelp lbl [] = []
zipWithIndexHelp lbl (val :: vals) = (val, lbl) :: zipWithIndexHelp (lbl + 1) vals

zipWithIndex :  Vect n a -> Vect n (a, Nat)
zipWithIndex = zipWithIndexHelp 0
