import Prelude
and_ :: [Bool] -> Bool
and_ [] = True
and_ (x:xs) = x && (and_ xs)


concat_ :: [[a]] -> [a]
concat_ [] = []
concat_ [x] = x
concat_ (x:xs) = x ++ concat_ xs

replicate_ :: Int -> a -> [a]
replicate_ 0 _ = []
replicate_ n x = x : replicate_ (n - 1) x

(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
(!!) (_:xs) n = (Main.!!) xs (n - 1)

elem_ :: Eq a => a -> [a] -> Bool
elem_ _ [] = False
elem_ e (x:xs) = e == x || elem_ e xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take(h) xs, drop(h) xs)
           where
            h = length(xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort( fst( halve xs))) (msort( snd( halve xs)) )