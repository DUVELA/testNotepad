import Data.Vect

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)


eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs

showExpr : Show ty => (op: String) -> (x : ty) -> (y : ty) -> String
showExpr op x y = "(" ++ show x ++ op ++ show y ++")"

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = showExpr " + " x y
  show (Sub x y) = showExpr " - " x y
  show (Mul x y) = showExpr " * " x y
  show (Div x y) = showExpr " / " x y
  show (Abs x) = "abs " ++ show (x)

(Neg ty, Integral ty, Abs ty, Eq ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

(Neg num, Integral num, Abs num, Eq num) => Cast (Expr num) num where
  cast orig = eval orig


Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)
