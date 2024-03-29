import Data.Vect
import DataStore

{- sample start-}
testStore1 : DataStore (SString .+. SString .+. SInt)
testStore1 = addToStore ("Mercury", "Mariner 10", 1974) $
            addToStore ("Venus", "Venera", 1961) $
            addToStore ("Uranus", "Voyager 2", 1986) $
            addToStore ("Pluto", "New Horizons", 2015) $
            empty

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty
{- sample end-}
{- TestStore start -}
-- export
listItems : DataStore schema -> List (SchemaType schema)
listItems input with (storeView input)
  listItems input | SNil = []
  listItems (addToStore value store) | (SAdd rec) = value :: listItems store | rec

filterKeys : (test : SchemaType val_schema -> Bool) -> DataStore (SString .+. val_schema) -> List String
filterKeys test input with (storeView input)
  filterKeys test empty | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec) = if test value then key :: filterKeys test store | rec
                                                                 else filterKeys test store | rec
{- TestStore end -}

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input = map snd (listItems input)
