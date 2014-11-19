--
--
evens xs = [ x | x <- xs, even x]
evens' = filter even
ex0 = sum . evens $ [827305 .. 927104]


squares  n = takeWhile (<=n^2) [ x^2 | x <- [1..]]
squares'' n = map (^2) [1..n]

sumSquares = sum . squares
ex4 = sumSquares 50

squares' n m = [x^2 | x<- [m+1..m+n]]
sumSquares' x= sum . uncurry  squares' $ (x,x)
ex5 = sumSquares' 50

ex6 = sum $ squares' 10 0

ex7 = sum $ squares' 0 10

coords n m = [ (x,y) | x <- [0..n], y <- [0..m]]
ex8 = foldr (-) 0 . map (uncurry (*)) $ coords 5 7
