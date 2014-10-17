double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
      a  = 10
      xs = [1..5]

last xs = head (reverse xs)

last' (x:[])    = x
last' (_:xs) = last' xs

init xs = take (length xs - 1) xs

init' (x:[]) = []
init' (x:xs) = x:(init' xs)

qsort [] = []
qsort xs = x : qsort larger ++ qsort smaller
  where x = maximum xs
        smaller = [a |a <- xs, a<x]
        larger = [b |b <- xs, b>= x]

