import DataStore
import Shape_abs

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues DataStore.empty | SNil = []
  getValues (addToStore (key, value) store) | (SAdd (key, value) store rec) 
   = value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

rectangle_area: Double -> Double -> Double
rectangle_area width height = width * height

data ShapeView : Shape -> Type where
     STriangle : (base: Double) -> (height: Double) -> ShapeView (Triangle base height)
     SRectangle : (length: Double) -> (height: Double) -> ShapeView (Rectangle length height)
     SCircle : (radius: Double) -> ShapeView (Circle radius)

shapeView : (s : Shape) -> ShapeView s
shapeView (Triangle base height) = STriangle base height
shapeView (Rectangle length height) = SRectangle length height
shapeView (Circle radius) = SCircle radius

area : Shape -> Double
area s with (shapeView s)
  area (Triangle base height) | (STriangle base height) = 0.5 * rectangle_area base height
  area (Rectangle length height) | (SRectangle length height) = rectangle_area length height
  area (Circle radius) | (SCircle radius) =  pi * radius * radius
