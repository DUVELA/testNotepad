import Data.Vect
import Data.String -- #1

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon k = Vect k Position

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

append : Vect n a -> Vect m a -> Vect (n + m) a
append xs ys = ?append_rhs

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

getStringOrInt : (isInt: Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

valToString : (isInt: Bool) -> StringOrInt isInt -> String
valToString False x = trim x -- #2
valToString True x = cast x

{-
#1 https://idris2.readthedocs.io/en/latest/typedd/typedd.html#chapter-6 

#2 import Data.String 를 하지 않고 :search String -> String을 하면 (trim이 안 보임)
Main> :search String -> String
prim__strTail : String -> String
prim__strReverse : String -> String
Builtin.fromString : String -> String
Prelude.reverse : String -> String
  Reverses the elements within a string.

  ```idris example
  reverse "ABC"
  ```
  ```idris example
  reverse ""
  ```
  Totality: total
  Visibility: public export
Prelude.Show.show : String -> String
Prelude.Interpolation.interpolate : String -> String

import Data.String 를 하고 :search String -> String을 하면 (trim등 다양한 게 추가로 보임)
Main> :search String -> String
prim__strTail : String -> String
prim__strReverse : String -> String
Builtin.fromString : String -> String
Prelude.reverse : String -> String
  Reverses the elements within a string.

  ```idris example
  reverse "ABC"
  ```
  ```idris example
  reverse ""
  ```
  Totality: total
  Visibility: public export
Prelude.Show.show : String -> String
Prelude.Interpolation.interpolate : String -> String
Data.String.trim : String -> String
  Trim whitespace on both sides of the string
  Totality: total
  Visibility: public export
Data.String.toUpper : String -> String
  Totality: total
  Visibility: public export
Data.String.toLower : String -> String
  Totality: total
  Visibility: public export
Data.String.strTail : String -> String
  Visibility: public export
Data.String.rtrim : String -> String
  Trim whitespace on the right of the string
  Totality: total
  Visibility: public export
Data.String.ltrim : String -> String
  Trim whitespace on the left of the string
  Totality: total
  Visibility: public export
-}
