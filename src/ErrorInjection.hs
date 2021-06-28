module ErrorInjection where

import qualified Config as C
import Types
import DataProcessing

import System.Random

polyBurstErrorInjection :: [Poly] -> Int -> [Poly]
polyBurstErrorInjection = undefined

burstErrorInjection :: Bits -> Int -> Bits
burstErrorInjection = undefined

polyBitErrorInjection :: [Poly] -> Double -> [Poly]
polyBitErrorInjection polys chance =
    bitsToPolys . (`bitErrorInjection` chance) . polysToBits $ polys

-- every bit has an equal chance to be flipped
-- 100 meaning 100% 0 meaning 0%
bitErrorInjection :: Bits -> Double -> Bits
bitErrorInjection bits chance = map flip bitChances where
    flip (bit, rand) = if rand < chance
                       then not bit
                       else bit
    bitChances = zip bits (randomList $ binToDec bits)

-- produce a list a random double given an integer seed
randomList :: Int -> [Double]
randomList seed = map (100*) $ randoms (mkStdGen seed) :: [Double]
