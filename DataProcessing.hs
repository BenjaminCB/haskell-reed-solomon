module DataProcessing where

import Types
import Config
import Data.Char

strToPoly :: String -> [Poly]
strToPoly = undefined

binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0

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
                   utfs      = map (toUTF . numToBin) charCodes
               in  concat utfs

numToBin :: Integral a => a -> Bits
numToBin n = if n == 0
             then [False]
             else numToBin' n
             where numToBin' n | n == 0 = []
                               | even n = numToBin' (div n 2) ++ [False]
                               | otherwise = numToBin' (div n 2) ++ [True]

toUTF :: Bits -> Bits
toUTF bs | length bs >= 8 = bs
         | otherwise        = toUTF $ False : bs
