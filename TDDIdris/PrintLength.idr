printInput : IO ()
printInput = do x <- getLine
                putStrLn x

printLength : IO ()
printLength = do putStr "Input string: "
                 input <- getLine
                 let len = length input
                 putStrLn (show len)

printLonger : IO ()
printLonger = putStr "First string: " >>= \_ => 
              getLine >>= \fst =>
              putStr "Second string: " >>= \_ =>
              getLine >>= \snd =>
              let flen = length fst in
              let slen = length snd in
              putStrLn (show((if (flen > slen) then flen else slen)))
{- chatGPT
지금 단계에서는 이렇게 정리하면 충분합니다.
\x => : 결과를 x라는 이름으로 사용한다.
\_ => : 결과는 오지만 사용하지 않는다.
 -}
