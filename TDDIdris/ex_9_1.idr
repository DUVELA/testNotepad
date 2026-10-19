import Decidable.Equality

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf: Last xs value) -> Last (x :: xs) value

last123 : Last [1, 2, 3] 3
last123 = LastCons (LastCons(LastOne))


isLast_rhs_0 : Last [] value -> Void
isLast_rhs_0 LastOne impossible
isLast_rhs_0 (LastCons prf) impossible

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No isLast_rhs_0
isLast (y :: ys) value = case decEq y value of 
                              Yes Refl => case ys of 
                                               [] => Yes LastOne
                                               z :: zs => case isLast ys y of
                                                           Yes prf => Yes (LastCons prf)
                                                           No notHere => --GPT
                                                              No (\prf => case prf of
                                                                 LastCons later => notHere later)
                              No ddt => case isLast ys value of
                                             Yes prf => Yes (LastCons prf)
                                             No notHere => --GPT
                                                No (\prf => case prf of
                                                      LastOne =>
                                                         ddt Refl

                                                      LastCons later =>
                                                         notHere later)
