module DataProcessing
    ( strToPoly
    , polyToStr
    , decToBin
    , binToDec
    , polysToBits
    , bitsToPolys)
    where

import Types
import qualified Config as C
import Data.Char
import qualified Util as U

---------------------------------------------------------------------------------
-- data processing for text to polynomial
---------------------------------------------------------------------------------
strToPoly :: String -> [Poly]
strToPoly = (`splitBinToPoly` C.k) . strToSplitBin

-- turn a sectioned list of bits strings into a set of polynomial
-- zeroes are padded until we can have a list of polynomials where they are all of size k
-- addZeroes should be optimized
splitBinToPoly :: [Bits] -> Int -> [Poly]
splitBinToPoly bs len = splitBinToPoly' (addZeroes $ map binToDec bs) [] where
    addZeroes bs = if length bs `mod` len == 0
                   then bs
                   else addZeroes (0 : bs)
    splitBinToPoly' bs ps
        | null bs   = reverse ps
        | otherwise = splitBinToPoly' (drop len bs) (take len bs : ps)

strToSplitBin :: String -> [Bits]
strToSplitBin = flip U.split C.m . strToBin

strToBin :: String -> Bits
strToBin = concatMap (toUTF . decToBin . ord)

---------------------------------------------------------------------------------
-- data processing for polynomial to text
---------------------------------------------------------------------------------
polyToStr :: [Poly] -> String
polyToStr ps = binToStr $ concatMap polyToBin ps

polyToBin :: Poly -> Bits
polyToBin = concatMap (toSymbolSize . decToBin)

binToStr :: Bits -> String
binToStr bs = map chr $ filter (/=0) $ map binToDec $ U.split bs 8

---------------------------------------------------------------------------------
-- rest of the functions
---------------------------------------------------------------------------------
binToBinStr :: Bits -> String
binToBinStr []         = ""
binToBinStr (True:bs)  = "1" ++ binToBinStr bs
binToBinStr (False:bs) = "0" ++ binToBinStr bs

binToDec :: Bits -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0

decToBin :: Integral a => a -> Bits
decToBin n = if n == 0
             then [False]
             else decToBin' n
             where decToBin' n | n == 0 = []
                               | even n = False : decToBin' (div n 2)
                               | otherwise = True : decToBin' (div n 2)

toSize :: Int -> Bits -> Bits
toSize n bs = bs ++ replicate (n - length bs) False

toUTF :: Bits -> Bits
toUTF = toSize 8

toSymbolSize :: Bits -> Bits
toSymbolSize = toSize C.m

bitsToPolys :: Bits -> [Poly]
bitsToPolys = (`splitBinToPoly` C.n) . (`U.split` C.m)

polysToBits :: [Poly] -> Bits
polysToBits = concatMap polyToBin
