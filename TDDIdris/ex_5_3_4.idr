import System.File.ReadWrite -- #1
import System.File.Handle

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "") 
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

listToString : List String -> String -- #2 
listToString [] = ""
listToString (x :: xs) = x ++ "\n" ++ listToString xs

readAndSave : IO ()
readAndSave = do putStrLn "Enter String (blank line to end):"
                 ls1 <- readToBlank
                 putStrLn "Enter file name (blank line to end):"
                 fileName <- getLine
                 if (fileName == "") -- #3
                    then putStrLn "You did not enter a file name."
                    else do putStrLn ("File name is " ++ fileName ++ ". " ++ "The list is " ++ show (ls1))
                            result <- writeFile fileName (listToString ls1) -- #4
                            case result of
                              Left err => putStrLn (show err)
                              Right () => putStrLn "Saved successfully."
{-
ChatGPT, Copilot 도움을 받아서 만듦.
#1 https://idris2.readthedocs.io/en/latest/typedd/typedd.html#chapter-5

#2 :t unlines 를 해보라는 chatGPT. 안 나온다고 하니 두 가지 선택지가 있었다.
"이 부분은 아직 책에서 안 배웠을 수도 있습니다.
그래서 지금은 두 가지 방법이 있습니다.
직접 만들어 보기 (재귀로 List String -> String)
또는
Prelude에 비슷한 함수가 있는지 찾아보기
예를 들어 REPL에서
:search List String -> String
또는
:search String -> String -> String
같이 검색해 보는 것도 Idris에서는 꽤 자주 하는 방법입니다." <- 지금 봤는데 내가 import를 안 했어도 보여 주는 게 신기함.

#3 다 하고 보니 Either라서 if를 굳이 안 써도 되겠구나 싶다.
  Main> :t writeFile
  System.File.ReadWrite.writeFile : HasIO io => String -> String -> io (Either FileError ())
  
#4 copilot 에게 writeFile 사용법을 물어봄.
-}
