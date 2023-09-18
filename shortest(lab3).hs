shortest :: [[a]] -> [a]

shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
                  then x
                  else shortest xs

type Poly = [Float]

sumPoly :: Poly -> Poly -> Poly

sumPoly [] x = x
sumPoly x [] = x
sumPoly (x:xs) (y:ys) = (x+y) (sumPoly xs ys)