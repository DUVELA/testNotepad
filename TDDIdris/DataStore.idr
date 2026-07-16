module Main
import System.REPL
import Data.Vect
import Data.String

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
     where
     addToData: Vect old String -> Vect (S old) String
     addToData [] = [newitem]
     addToData (item :: items) = item :: addToData items

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit
parseCommand : (cmd: String) -> (args: String) -> Maybe Command
parseCommand "add" str = Just(Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just(Get (cast (val)))
parseCommand "size" "" = Just Size
parseCommand "search" word = Just(Search word)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing
     
parse : (input : String) -> Maybe Command
parse input = case span (/=' ') input of
                    (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos: Integer) -> (store: DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                               case integerToFin pos (size store) of
                               Nothing => Just ("Out of range\n", store)
                               Just id => Just (index id store_items ++ "\n", store)

searchHelp : String -> DataStore -> Maybe (String, DataStore)
searchHelp str store = let result = dtest 0 str store in
                           if result == "" then Just ("Not found\n", store)
                           else Just (result, store)
                       where
                           dtest : Nat -> String -> DataStore -> String
                           dtest k str (MkData 0 []) = ""
                           dtest k str (MkData (S len) (x :: xs)) = case isInfixOf str x of
                                   True => show (k) ++ ": " ++ x ++ "\n" ++ dtest (S k) str (MkData len xs)
                                   False => dtest (S k) str (MkData len xs)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos) => getEntry pos store
                              Just Size => Just (show (size store) ++ "\n", store)
                              Just (Search str) => searchHelp str store 
                              Just Quit => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput

{-
chatGPT에게 - search 를 만들 때 size로 계산하고, getEntry를 이용하려고 했을 때 막히다가 len을 쓰기까지 도착 후 원하는 건 앞에서 부터 0 으로 시작하는 거였는데 - scala? 에서 Type 부분에 값을 직접 넣었던 것 같아서 물어봄.
그래서 제가 계속 "번호를 들고 다닌다"고 했던 것입니다.
- let ... = ... in 이해했다고 생각했는데 잊어버림.
-}
