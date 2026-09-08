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

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

(Show num) => Show (Expr num) where
      show (Val num1) = show num1
      show (Add num1 num2) = "(" ++ show num1 ++ " + " ++ show num2 ++ ")"
      show (Sub num1 num2) = "(" ++ show num1 ++ " - " ++ show num2 ++ ")"
      show (Mul num1 num2) ="(" ++ show num1 ++ " * " ++ show num2 ++ ")"
      show (Div num1 num2) = "(" ++ show num1 ++ " `div` " ++ show num2 ++ ")"
      show (Abs num1) = show num1

{-
(Show num, Abs num, Neg num, Integral num) => Show (Expr num) where
      show (Val num) = show (eval (Val num))
      show (Add num1 num2) = show (eval (Add num1 num2))
      show (Sub num1 num2) = show (eval (Sub num1 num2))
      show (Mul num1 num2) = show (eval (Mul num1 num2))
      show (Div num1 num2) = show (eval (Div num1 num2))
      show (Abs num) = show (eval (Abs num))
 -}
