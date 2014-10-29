-- list comprehension
import Data.Char

sum100 = sum [ x^2 | x <- [1..100] ]

replicate n a = [ a | _ <- [1..n] ]

pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]


perfects n = [ x | x <- [1..n], isPerfect x ]
  where isPerfect x = x*2 == (sum $ factors x)
        factors n = [ p | p <- [1..n], n `mod` p == 0 ]
        
find k t = [ v | (k', v) <- t , k == k']
positions x xs = find x $ zip xs [1..]

scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys ]

-- caesar
ref4chr c
  | isLower c = ord 'a'
  | otherwise = ord 'A'
let2int c = ord c - ref4chr c
int2let i = chr (ord 'a' + i)

shift n c
  | c == ' ' = ' '
  | otherwise = int2let ((let2int c + n) `mod` 26)

encode n xs = [shift n x | x <- xs ]

xs = 1 : [ x+1 | x <- xs ]

riffle xs ys = [ [x,y] | (x,y) <- zip xs ys]


mexp m 0 = 1
mexp m n = m * mexp m (n-1)
