--idris2, vscode, wsl
import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S (my_length xs)

my_reverse : List a -> List a
my_reverse xs = revAcc [] xs
  where
    revAcc : List a -> List a -> List a
    revAcc acc [] = acc
    revAcc acc (head :: tail) = revAcc (head :: acc) tail

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vect_map : (String -> Nat) -> Vect n String -> Vect n Nat
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
