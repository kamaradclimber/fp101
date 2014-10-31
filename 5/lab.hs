-- lab

toDigits :: Integer -> [Integer]
toDigits n
  | n < 10 = [n]
  | otherwise = toDigits p ++ [q]
    where (p,q) = n `divMod` 10

toDigitsRev n = reverse $ toDigits n

doubleSeconds []        = []
doubleSeconds (p:q:qs)  = p: (2*q) : doubleSeconds qs
doubleSeconds ns        = ns

sum' n p = n + sum (toDigits p)
sumDigits = foldl sum' 0

isValid n = mod (sumDigits . doubleSeconds . toDigitsRev $ n) 10 == 0
