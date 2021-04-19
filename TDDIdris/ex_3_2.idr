plusOne : (x : Nat) ->  Nat
plusOne x = x + 1

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = plusOne (my_length xs)
