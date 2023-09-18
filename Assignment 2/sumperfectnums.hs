-- Finds the sum of all perfect numbers under 1,000,000,000,000

-- Creates a list using list comprehension this list will be [6,28,496,8128,33550336,8589869056,137438691328] which will be added using sum
-- and evaluates to 146062119378.

-- takewhile ensures that the perfect numbers are all under 1,000,000,000,000
-- filter takes the ll command to find all of the exponents of Mersenne primes
sumperfect :: Int
sumperfect = sum (takeWhile(<1000000000000) [2^(x - 1)*((2^x)-1) | x <- filter (ll) [2,3..]])

-- returns True if the number is a prime and false otherwise
-- calculates the length of the list of factors of a number, if it is 1 then it is only divisible
-- by 1 and itself and is thereforee a prime
isprime :: Int -> Bool
isprime num = length (factors num) == 1

-- finds the number of factors of a given number
factors :: Int -> [Int]
factors num = [x | x <- [1..(floor (sqrt (fromIntegral num)))], num `rem` x == 0]

-- Uses the Lucas-Lehmer test to calculate if a number is a Mersenne prime
-- https://en.wikipedia.org/wiki/Lucas%E2%80%93Lehmer_primality_test
ll :: Int -> Bool
ll 2 = True
ll num = isprime num && s (num-2) == 0
    where
        mers = (2^num) - 1
        s 0 = 4
        s num = 
            let n = s (num-1)
                in ((n^2)-2) `rem` mers
