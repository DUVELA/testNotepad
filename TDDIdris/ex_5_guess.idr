module Main
import System

readNumber : IO (Maybe Nat)
readNumber = do 
   input <- getLine
   if all isDigit (unpack input)
      then pure (Just (cast input))
      else pure Nothing

guess : (target: Nat) -> IO ()
guess target = do putStrLn "Enter guess number: "
                  Just userNum <- readNumber | Nothing => do putStrLn "Invalid input"
                                                             guess target
                  if (target < userNum) then do
                     putStrLn "Too high"
                     guess target
                     else if (target > userNum) then do
                           putStrLn "Too low"
                           guess target
                     else putStrLn "Correct!"

guessAttempts : (target: Nat) -> (guesses: Nat) -> IO ()
guessAttempts target guesses = do putStrLn "Enter guess number: "
                                  Just userNum <- readNumber | Nothing => do putStrLn "Invalid input"
                                                                             guessAttempts target (S guesses)
                                  if (target < userNum) then do
                                     putStrLn "Too high"
                                     guessAttempts target (S guesses)
                                     else if (target > userNum) then do
                                          putStrLn "Too low"
                                          guessAttempts target (S guesses)
                                     else putStrLn ("Correct! Attempts: " ++ show (S guesses))

{- chatGPT의 도움을 받고 만드는 중.
남음: random number between 1 and 100 and then calls guess. 다음엔 1 부터 100 중 임의의 숫자 만들기.

if then else ->
  ㄴ if then do //재귀는 "계속해야 하는 경우"에만 한다.
        else

putStrLn ("Correct!" ++ show (guesses)) ->
  ㄴputStrLn ("Correct!" ++ show (S guesses)) -> //정답도 '시도'
    ㄴputStrLn ("Correct! Attempts: " ++ show (S guesses)) //조금 읽기 쉽게 ... 처럼 써도 좋습니다. 그러면 "Correct! Attempts: 4" 처럼 나오니까요.
   
guessz -> guessAttempts

...Nothing => do putStrLn "Invalidinput" ->
  ㄴ...Nothing => do putStrLn "Invalidinput"
                     guess target // 사용자 입장에서는 ... 처럼 다시 입력받는 것이 자연스럽습니다.
-}
