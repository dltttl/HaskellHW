module Intro where

-- Double factorial
doubleFact :: Integer -> Integer
doubleFact n = if (n == 1) then 1 else if (n == 2) then 2 else n * doubleFact (n-2)


-- Sequence b0​=1; b1​=2;b2​=3; bk+3​=bk+2​−2bk+1​+3bk
seqB :: Integer -> Integer
seqB n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | otherwise = helperSeq 3 3 2 1 n

helperSeq i a b c n | i == n = ans
                 | otherwise = helperSeq (i+1) (ans) a b n
                 where ans = a-2*b+3*c


--Fibonacci extended
fibonacci :: Integer -> Integer
fibonacci n | n==0 = 0
            | (abs n)==1 = 1
            | n > 0 = helper 0 1 n
            | n < 0 = (-1)^(abs(n)+1) * (helper 0 1 (abs n))
            
helper prev curr n | n==1 = curr
                   | otherwise = helper curr (prev+curr) (n-1)


--Sum and Count for List
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x==0 = (0,1)
              | otherwise = (sum crutch, toInteger (length crutch))
                  where crutch = digits x


digits x | x==0 = []
         | x<0 = digits (-x)
         | otherwise = digits(x `div` 10) ++ [x `mod` 10]


--Integral
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let h = (b-a)/1000
                        in summator 0 f a h 0

summator s f a h n | n==1000 = s
                   | otherwise = summator (s + h * f(nextStep)) f a h (n+1)
                        where nextStep = a + h*(n + 0.5)