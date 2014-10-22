import Prelude hiding ((||))
import Prelude hiding ((&&))
-- 3rd course
--
safetail xs
  | null xs = []
  | otherwise = tail xs

safetail' xs
  | length xs == 0 = []
  | otherwise = tail xs

safetail'' [] = []
safetail'' xs = tail xs


--False || False = False
--_     || _     = True

--False || b = b
--True  || _ = True

--b || c
--  | b == c    = b
--  | otherwise = True

b || False = b
_ || True  = True

a && b = if a then b else a

remove n xs = take n xs ++ drop (n+1) xs

--e2 :: [[[Integer]]]
--e2 :: [[[Int]]]
--e2 :: Num a => [[[a]]]
e2 = [[[1]], [[2]]]

e13 :: Int -> Int -> Int
e13  x y = x + y * y
--e13  x y = x * x - x
