-- Lab Session 2
-- 
-- Sample answers

triangleArea :: (RealFloat a) => a -> a-> a-> a

triangleArea a b c = let s = (a+b+c)/2
                                in sqrt (s * (s-a) * (s-b) * (s-c))


isSum :: Int -> Int -> Int -> Bool

isSum x y z
     | x+y == z = True
     | x+z == y = True
     | y+z == x = True
     | otherwise = False



validTriangle :: (RealFloat a) => a -> a-> a-> Bool

validTriangle x y z
     | x+y < z = False
     | x+z < y = False
     | y+z < x = False
     | otherwise = True

triangleArea2 :: (RealFloat a) => a -> a-> a-> a

triangleArea2 a b c = if (validTriangle a b c)
                      then let s = (a+b+c)/2
                           in sqrt (s * (s-a) * (s-b) * (s-c))
                      else error "Not a triangle!"