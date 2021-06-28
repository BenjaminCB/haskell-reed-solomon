module Util where

insert :: a -> Int -> [a] -> [a]
insert x i xs = take i xs ++ [x] ++ drop (i + 1) xs
