-- high order functions
--

mapf f p xs = map f (filter p xs)

all' p = not . any (not .p)
all'' p xs = foldl (&&) True (map p xs)
all''' p = foldr (&&) True . map p


-- does not work on infinite lists
any' p xs = length (filter p xs) > 0

any'' p = not . null . dropWhile (not . p)
any''' p xs = foldr (\ x acc -> (p x) || acc) False xs


compose = foldr (.) id
sumpsqeven = compose [sum, map (^2), filter even]
