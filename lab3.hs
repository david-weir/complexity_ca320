-- Q1
isPal :: (Eq a) => [a] -> Bool

isPal x = if x == reverse x
          then True
          else False

-- Q2
shortest :: [[a]] -> [a]

shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
                  then x
                  else shortest xs

-- Q3

type Poly = [Float]

sumPoly :: Poly -> Poly -> Poly

sumPoly [] x = x
sumPoly x [] = x
sumPoly (x:xs) (y:ys) = (x+y):(sumPoly xs ys)

-- Q4
evalPoly :: Int -> [Int] -> Int

evalPoly _ [p] = p
evalPoly x (y:ys) = y + (x * (evalPoly x ys))


