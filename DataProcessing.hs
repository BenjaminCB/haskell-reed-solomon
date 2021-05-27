module DataProcessing where

import Types
import Config
import Data.Char

strToPoly :: String -> [Poly]
strToPoly str = reverse $ addZeroes $ splitBinToPoly $ strToSplitBin str where
    addZeroes (x:xs) | length x < k = addZeroes ((0:x) : xs)
                     | otherwise    = x:xs

splitBinToPoly :: [Bits] -> [Poly]
splitBinToPoly bs = splitBinToPoly' (map binToDec bs) [] where
    splitBinToPoly' [] ps = ps
    splitBinToPoly' es ps = splitBinToPoly' (drop k es) (take k es : ps)

binToDec :: Bits -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0

binToStr :: Bits -> String
binToStr []         = ""
binToStr (True:bs)  = "1" ++ binToStr bs
binToStr (False:bs) = "0" ++ binToStr bs

strToSplitBin :: String -> [Bits]
strToSplitBin = flip splitBin m . strToBin

splitBin :: Bits -> Int -> [Bits]
splitBin bs n =
    if mod (length bs) n /= 0
    then error "splitBin error"
    else splitStr' bs n [] where
        splitStr' [] _ bss = bss
        splitStr' bs n bss = splitStr' (drop n bs) n (take n bs : bss)

strToBin :: String -> Bits
strToBin str = let charCodes = map ord str
                   utfs      = map (toUTF . decToBin) charCodes
               in  concat utfs

decToBin :: Integral a => a -> Bits
decToBin n = if n == 0
             then [False]
             else decToBin' n
             where decToBin' n | n == 0 = []
                               | even n = False : decToBin' (div n 2)
                               | otherwise = True : decToBin' (div n 2)

toUTF :: Bits -> Bits
toUTF bs | length bs >= 8   = bs
         | otherwise        = toUTF $ False : bs
