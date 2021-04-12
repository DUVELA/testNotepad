palindrome : Nat -> String -> Bool
palindrome x str = length str > x && toLower str == toLower(reverse str)

counts : String -> (Nat, Nat)
counts str = (size(words str), length str)

top_ten : Ord a => List a -> List a
top_ten la = take 10 (reverse (sort la))

over_length : Nat -> List String -> Nat
over_length na ls = size (filter ( > na) (map length ls))
