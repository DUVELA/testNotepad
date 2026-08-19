module Main
import System.REPL
import Data.Vect
import Data.String

export infixr 5 .+.
data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

data Command : Schema -> Type where
     SetSchema : (newschema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     GetAll : Command schema
     Quit : Command schema
 
record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData shadowingSchema size store) newitem 
           = MkData shadowingSchema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType shadowingSchema) -> Vect (S oldsize) (SchemaType shadowingSchema) -- schema is shadowing Main.DataStore.schema
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items

display : {schema : _} -> SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in 
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (display (index id (items store)) ++ "\n", store)

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
   where
      getQuoted : List Char -> Maybe (String, String)
      getQuoted ('"' :: xs) 
         = case span (/= '"') xs of
                (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                _ => Nothing
      getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = case unpack input of
                               [] => Nothing
                               h :: rest => Just (h, ltrim (pack rest))
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of 
                                               Nothing => Nothing
                                               Just (l_val, input') => 
                                                case parsePrefix schemar input' of
                                                     Nothing => Nothing
                                                     Just (r_val, input'') => 
                                                          Just((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of 
                                    [] => Just SString
                                    _ => case parseSchema xs of
                                              Nothing => Nothing
                                              Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _ => case parseSchema xs of
                                           Nothing => Nothing
                                           Just xs_sch => Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs) = case xs of 
                                  [] => Just SChar
                                  _ => case parseSchema xs of
                                            Nothing => Nothing
                                            Just xs_sch => Just (SChar .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" rest = parseSchema (words rest) >>= \schemaok =>
                                       Just (SetSchema schemaok)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restok => Just (Add restok)

parseCommand schema "get" "" = Just (GetAll)
parseCommand schema "get" val = case all isDigit (unpack val) of
                                     False => Nothing
                                     True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing

parse : (schema : Schema) -> (input: String) -> Maybe(Command schema)
parse schema input = case span (/= ' ') input of 
                          (cmd, args) => parseCommand schema cmd (ltrim args)

ddt : DataStore -> Nat -> String -- #2
ddt (MkData schema 0 []) k = ""
ddt (MkData schema (S len) (x :: xs)) k = 
    show (k) ++ ": " ++ display x ++ "\n"
    ++ ddt (MkData schema len xs) (S k)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (SetSchema schema') => case setSchema store schema' of
                                                                 Nothing => Just ("Can't update schema\n", store)
                                                                 Just store' => Just ("OK\n", store')
                                Just (Get pos) => getEntry pos store
                                Just (GetAll) => Just(ddt store 0, store)
                                Just Quit => Nothing
maybeddt : Maybe Int -> Maybe Int -> Maybe Int
maybeddt mx my = mx >>= \x_val => 
                 my >>= \y_val => 
                 Just (x_val + y_val)

maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd mx my = do x_val <- mx
                    y_val <- my
                    Just (x_val + y_val)

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
{-
#1 
Char를 위해 search를 해봄.
Main> :search String-> Char
prim__strHead : String -> Char
copilot에 Char가 어디에 들어가는 게 순서상 좋을지 물어보니 SChar란 이름과 Schema 부터 display 등에 넣을 것을 추천함. 순서는 SInt 다음.

- 다음은 두 번째 get만 했을 때 전체 내용이 나오도록 하기. (o) 2026-08-19
#2 ddt는 GetAll을 위해서. 
>이건 그냥 코드를 외운 게 아니라 "DataStore 안에 실제로 뭐가 들어 있고, 그것을 어떤 구조로 꺼내 볼 수 있는가"를 Idris에게 물어본 것입니다.
>그리고 아주 중요한 에러도 하나 제대로 만났습니다.
0부터 1씩 커지는 걸 하기 위해서 Nat Type인 k를 넣어주고 processInput에서 Just (GetAll) => Just(ddt store 0, store) 로 써줬다.

세 번째 업데이트는...
-}
