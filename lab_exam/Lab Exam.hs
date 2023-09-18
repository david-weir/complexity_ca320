-- Question 1

ans1 = [x | x <- [1,3..100], x `mod` 3 == 0]

-- Question 2

sum_diff :: Int -> Int -> Int

sum_diff x y = if (x `mod` 2 /= 0) || (y `mod` 2 /= 0)
          then x + y
          else x - y

-- Question 3

join :: [Int] -> [Int] -> [Int]

join [] x = x
join (x:xs) ys = if x `elem` ys
                 then join xs ys
                 else x:(join xs ys)

-- Question 4

data BinTree t = Empty | Root t (BinTree t) (BinTree t)
    deriving (Show)

-- Question 5
preorder :: BinTree a -> [a]

preorder Empty = []
preorder (Root x Empty Empty) = [x]
preorder (Root a left right) = [a] ++ (preorder left) ++ (preorder right)