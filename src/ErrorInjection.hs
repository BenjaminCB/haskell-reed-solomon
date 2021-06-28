module ErrorInjection where

import Config
import Types

import System.Random

-- every bit has an equal chance to be flipped
bitErrorInjection :: Bits -> Int -> Bits
bitErrorInjection bits chance = undefined

-- produce a list a random double given an integer seed
randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed) :: [Double]
