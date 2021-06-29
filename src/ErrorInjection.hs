module ErrorInjection
    ( polyBurstErrorInjection
    , polyBitErrorInjection )
    where

import qualified Config as C
import Types
import DataProcessing
import qualified Util as U

import System.Random
import Data.Bits (xor)

-- Every block will have a certain amount of error.
-- the generator could be done better
polyBurstErrorInjection :: [Poly] -> Int -> [Poly]
polyBurstErrorInjection polys errAmount = map errors polys where
    errors poly =
        let blockLen = length . head $ polys
            gen = sum poly
            take' = take errAmount
            errPoss = take' $ randomMaxMin 0 (fromIntegral $ blockLen - 1) gen
            errVals = take' $ randomMaxMin 0 (2 ^ C.m - 1) gen
        in  U.insertAll errVals errPoss poly

polyBitErrorInjection :: [Poly] -> Double -> [Poly]
polyBitErrorInjection polys chance =
    bitsToPolys . (`bitErrorInjection` chance) . polysToBits $ polys

-- every bit has an equal chance to be flipped
-- 100 meaning 100% 0 meaning 0%
bitErrorInjection :: Bits -> Double -> Bits
bitErrorInjection bits chance = map flip bitChances where
    flip (bit, rand) = if 100 * rand < chance
                       then not bit
                       else bit
    bitChances = zip bits (randomList $ binToDec bits)

-- produce a list a random double given an integer seed
randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed) :: [Double]

-- produce a list of randoms integers with given a uppor bound lower bound and seed
randomMaxMin :: Double -> Double -> Int -> [Int]
randomMaxMin upper lower seed =
    map (fromIntegral . floor . (\x -> x * (upper - lower + 1) + lower)) (randomList seed)

-- Takes a list of polynomials and tries to create something that is seemingly random
-- With this i can get close to the desired result without having to touch IO
polysToPsuedoRandomGen :: [Poly] -> Int
polysToPsuedoRandomGen = foldr (\a b -> sum a `xor` b) 0
