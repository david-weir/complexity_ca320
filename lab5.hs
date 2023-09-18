data BinTree t = Empty | Root t (BinTree t) (BinTree t)
    deriving (Eq, Ord, Show)
    
leaf x = Root x Empty Empty
myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))(Root 7 Empty Empty)

addnode :: Ord a => a -> BinTree a -> BinTree a

addnode a Empty = leaf a
addnode x (Root a left right)
    | x < a = Root a (addnode x left) right
    | otherwise = Root a left (addnode x right)

maketree :: Ord a => [a] -> BinTree a

maketree [] = Empty
maketree [x] = leaf x
maketree (x:xs) = addnode x (maketree xs)

inorder :: BinTree a -> [a]

inorder Empty = []
inorder (Root x Empty Empty) = [x]
inorder (Root x left right) = inorder left ++ [x] ++ inorder right

mpsort :: Ord a => [a] -> [a]
mpsort x = inorder (maketree x)
