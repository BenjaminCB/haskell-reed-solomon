module DataProcessing where

import Types
import Config
import Data.Char

---------------------------------------------------------------------------------
-- data processing for text to polynomial
---------------------------------------------------------------------------------
strToPoly :: String -> [Poly]
strToPoly str = reverse $ addZeroes $ splitBinToPoly $ strToSplitBin str where
    addZeroes (x:xs) | length x < k = addZeroes ((0:x) : xs)
                     | otherwise    = x:xs

splitBinToPoly :: [Bits] -> [Poly]
splitBinToPoly bs = splitBinToPoly' (map binToDec bs) [] where
    splitBinToPoly' [] ps = ps
    splitBinToPoly' es ps = splitBinToPoly' (drop k es) (take k es : ps)

strToSplitBin :: String -> [Bits]
strToSplitBin = flip splitBin m . strToBin

strToBin :: String -> Bits
strToBin str = let charCodes = map ord str
                   utfs      = map (toUTF . decToBin) charCodes
               in  concat utfs

splitBin :: Bits -> Int -> [Bits]
splitBin bs n =
    if mod (length bs) n /= 0
    then error "splitBin error"
    else splitStr' bs n [] where
        splitStr' [] _ bss = reverse bss
        splitStr' bs n bss = splitStr' (drop n bs) n (take n bs : bss)

---------------------------------------------------------------------------------
-- data processing for polynomial to text
---------------------------------------------------------------------------------
polyToStr :: Poly -> String
polyToStr = binToStr . polyToBin

polyToBin :: Poly -> Bits
polyToBin p = concat $ map (toSymbolSize . decToBin) p

binToStr :: Bits -> String
binToStr bs = map (chr . binToDec) $ splitBin bs 8

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

toUTF :: Bits -> Bits
toUTF bs = bs ++ replicate (8 - length bs) False

toSymbolSize :: Bits -> Bits
toSymbolSize bs = bs ++ replicate (m - length bs) False
