data Shape = ||| A triangle, with its base length and height
			 Triangle Double Double
			| ||| A rectangle, with its length and height
			 Rectangle Double Double
			| ||| A circle, with its radius
			 Circle Double
%name Shape shape, shape1, shape2

data Picture = Primitive Shape
			 | Combine Picture Picture
			 | Rotate Double Picture
			 | Translate Double Double Picture
%name Picture pic, pic1, pic2

data Tree elem = Empty
			   | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1

data Expr = Val Int
		  | Add Expr Expr
		  | Sub Expr Expr
		  | Mult Expr Expr

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => Node left val right
                                      GT => Node left val (insert x right)


listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = let lTTree = listToTree xs in
                        insert x lTTree


treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node tree x tree1) = treeToList tree ++ [x] ++ treeToList tree1

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate (x) + evaluate (y)
evaluate (Sub x y) = evaluate (x) - evaluate (y)
evaluate (Mult x y) = evaluate (x) * evaluate (y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe x Nothing = x
maxMaybe x (Just y) =  case compare x (Just y) of
                            LT => Just y
                            EQ => x
                            GT => x


area : Shape -> Maybe Double
area (Triangle base height) = Just (0.5 * base * height)
area (Rectangle length height) = Nothing
area (Circle radius) = Nothing

biggestTriangle: Picture -> Maybe Double
biggestTriangle (Primitive shape) = area shape
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
