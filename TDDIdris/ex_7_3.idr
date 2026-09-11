import Data.Vect
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger
Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
Abs ty => Abs (Expr ty) where
    abs = Abs

Foldable Expr where
   foldr func acc (Val x) = func x acc
   foldr func acc (Add x y) = foldr func (foldr func acc x) y
   foldr func acc (Sub x y) = foldr func (foldr func acc x) y
   foldr func acc (Mul x y) = foldr func (foldr func acc x) y
   foldr func acc (Div x y) = foldr func (foldr func acc x) y
   foldr func acc (Abs x) = foldr func acc x
   
Functor Expr where
   map f (Val x) = Val (f x)
   map f (Add x y) = Add (map f x) (map f y)
   map f (Sub x y) = Sub (map f x) (map f y)
   map f (Mul x y) = Mul (map f x) (map f y)
   map f (Div x y) = Div (map f x) (map f y)
   map f (Abs x) = Abs (map f x)

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

{- 
Main> map (*2) (the (Expr _) (1 + 2 * 3))
Add (Val 2) (Mul (Val 4) (Val 6))

Main> map show (the (Expr _) (1 + 2 * 3))
Error: Can't find an implementation for Num (Expr ?a).

(Interactive):1:25--1:34
 1 | map show (the (Expr _) (1 + 2 * 3))
                             ^^^^^^^^^

Main> map show (the (Expr Integer) (1 + 2 * 3))
Add (Val "1") (Mul (Val "2") (Val "3"))
-}
