module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action :: Concurrent a -> Action
action (Concurrent c) = compute c 

unit _ = Stop
compute c = c unit


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent (\_ -> Stop)


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom ioa = Concurrent (atom' ioa)

atom' ioa c = Atom $ (ioa >>= \a -> return $ c a)


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork concuA = Concurrent (\contU -> Fork (action concuA) (contU ())) 


par :: Concurrent a -> Concurrent a -> Concurrent a
par cA cB = Concurrent (\c -> Fork (action cA) (action cB))  


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = Concurrent (bind f g)
    return x = Concurrent (\c -> c x)
-- bind :: ((a -> Action) -> Action) -> (a -> (c-> Action) -> Action) -> ((c -> Action ) -> Action)
bind ma g = \bAction -> ma $ \a -> (extract' $ g a) bAction

extract' (Concurrent gg) = gg 


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin  [] = return ()
roundRobin (ac:actions) = execute ac >>= (\res -> roundRobin (actions ++ res))

execute :: Action -> IO [Action] 
execute Stop = return []
execute (Atom ioa) = ioa >>= (\a -> return [a])
execute (Fork a b) = return [a,b]
                             

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

