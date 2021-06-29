module Config where

m = 8 :: Int                -- Symbol size
n = 255 :: Int              -- Block size
k = 239 :: Int              -- Message size
t = div (n - k) 2 :: Int    -- Number of correctable symbols
g = 285 :: Int              -- Field generator
