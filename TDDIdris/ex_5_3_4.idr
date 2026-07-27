import System.File.ReadWrite -- #1
import System.File.Handle
import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "") 
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

listToString : List String -> String -- #2 import Data.String (:t unlines \n Data.String.unlines : List String -> String)
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

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do sample <- openFile filename Read
                           case sample of
                              Left err => do putStrLn (show err)
                                             pure (0 ** [])
                              Right h => readLine h [] 

                           where
                              readLine : {m: Nat} -> File -> Vect m String -> IO (n : Nat ** Vect n String) -- #5
                              readLine file acc = do eof <- fEOF file
                                                     if eof then do
                                                        closeFile file
                                                        putStrLn (show acc)
                                                        pure (_ ** acc)
                                                      else do
                                                         line <- fGetLine file
                                                         case line of
                                                            Left err => do putStrLn (show err)
                                                                           pure (0 ** [])
                                                            Right ddt => readLine file (acc ++ [ddt])

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
#5 -

ChatGPT : 
그래서 저는 앞으로는 이 방식을 추천합니다.
문제를 먼저 읽고 직접 설계한다.
모르는 함수만 검색한다.
그래도 연결이 안 되면 저에게 질문한다.
이 방법이 가장 오래 기억에 남고, 지금까지의 대화를 보면 사용자분에게도 잘 맞는 학습 방식인 것 같습니다.
-}
