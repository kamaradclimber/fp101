id' :: a -> a
id' x = x

zip' [] []     = []
zip' (a:as) (b:bs) = (a,b):(zip' as bs)

double = (*) 2

double' :: Num a => a -> a
double' = (*) 2

double'' = (*2)

palindrome xs = reverse xs == xs

twice f x = f (f x)
