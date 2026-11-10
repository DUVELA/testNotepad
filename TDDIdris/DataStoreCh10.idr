module DataStore
import Data.Vect
private -- 에러가 나서 추가.
infixr 5 .+.
public export
data Schema = SString | SInt | (.+.) Schema Schema
public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)
export
record DataStore (schema: Schema) where
   constructor MkData
   size : Nat
   items : Vect size (SchemaType schema)

export
empty : DataStore schema
empty = MkData 0 []

export
addToStore : (value : SchemaType schema) -> (store : DataStore schema) ->
              DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

public export
data StoreView : (schema : _) -> DataStore schema -> Type where
     SNil : StoreView schema DataStore.empty
     SAdd : (entry, store : _) -> (rec : StoreView schema store) ->
            StoreView schema (addToStore entry store)


storeViewHelp : {size : _} -> (items : Vect size (SchemaType schema)) ->
                StoreView schema (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (val :: xs) = SAdd val (MkData _ xs) (storeViewHelp xs) -- 라이브러리에 따로 이야기 없던데 SAdd가 3 개를 받아서 책 그대로 하면 에러 난다고 gemini가 알려줌.

export
storeView : (store : DataStore schema) -> StoreView schema store
storeView (MkData size items) = storeViewHelp items
