import System

readNumber: IO (Maybe(Nat))
readNumber = do input <- getLine
                if all isDigit (unpack input)
                    then pure (Just (cast input))
                    else pure Nothing

guess: (target: Nat) -> (guesses: Nat) -> IO ()
guess target guesses = do putStrLn (show guesses ++ " guesses so far")
                          putStr ("Guess a number 1 to 45: ")
                          isNum <- readNumber
                          case isNum of
                                Nothing => do putStrLn "Invalid input"
                                              guess target guesses
                                Just userguess => case compare target userguess of
                                                        LT => do putStrLn "Too hight"
                                                                 guess target (guesses + 1)
                                                        EQ => putStrLn "Well done!"
                                                        GT => do putStrLn "Too low"
                                                                 guess target (guesses + 1)

                    
main : IO ()
main = do t <- time
          guess (cast ((t `mod` 45) + 1)) 0

                    
