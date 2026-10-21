import Decidable.Equality
import Data.Vect

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     MkWordState : (word : String) ->
                   (missing : Vect letters Char) ->
                   WordState guesses_remaining letters

data Finished : Type where
     Lost : (game : WordState 0 (S letters)) -> Finished
     Won : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]

{-game : {guesses : _} -> {letters : _} ->
       WordState (S guesses) (S letters) -> IO Finished -}
game : WordState (S guesses) (S letters) -> IO Finished
game x = ?game_rhs

isValidString : (s: String) -> Dec (ValidInput (unpack s))
isValidString s = ?isValidString_rhs

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               x <- getLine
               case isValidString (pack (map toUpper (unpack x))) of
                    Yes prf => pure (_ ** prf)
                    No contra => ?readGuess_rhs
