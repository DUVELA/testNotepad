import Data.Stream

take : Nat -> List a -> List a
take 0 xs = []
take (S k) [] = []
take (S k) (x :: xs) = x :: take k xs

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)
