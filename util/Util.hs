module Util where

insertAll :: [a] -> [Int] -> [a] -> [a]
insertAll [] _ as = as
insertAll _ [] as = as
insertAll (v:vals) (p:poss) as = insertAll vals poss (insert v p as)

insert :: a -> Int -> [a] -> [a]
insert x i xs = take i xs ++ [x] ++ drop (i + 1) xs

-- split a list into a list of sections
-- might need an error case if we can't have an even number of sections
split :: [a] -> Int -> [[a]]
split b n = splitBin' b [] where
    splitBin' [] bs = bs
    splitBin' b  bs = let len = length b - n
                      in  splitBin' (take len b) (drop len b : bs)

