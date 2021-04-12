module Main

wantTest: String -> (Bool, (Nat, Nat))
wantTest str = (realPalindrome str, allLengths str)
  where
    realPalindrome : String -> Bool
    realPalindrome str = toLower str == toLower(reverse str)
    allLengths : String -> (Nat, Nat)
    allLengths str = (size(words str), length str)

showPalindrome: String -> String
showPalindrome str = show (wantTest str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showPalindrome
