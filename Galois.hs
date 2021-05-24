module Galois where

-------------------------------------------------------------------------------
----should start using maybe as -1 in the multiply step gives wierds results---
-------------------------------------------------------------------------------

import Data.Bits
import Data.List

type Element = Int
type Poly = [Element]

m = 4
n = 15
k = 11
t = div (n - k) 2
g = 2 ^ 4 + 2 ^ 3 + 1 :: Int
toPoly = initToPoly g
toIndex = initToIndex'


initToPoly :: Element -> [Element]
initToPoly gen = let size = 2 ^ m
                     hb   = highestBit gen
                     init is _ 0 = reverse is
                     init is c i = let c' = shift c 1
                                       c'' = xor c' gen
                                   in if hasBit c' hb
                                      then init (c'':is) c'' (i - 1)
                                      else init (c':is) c' (i - 1)
                     in init [1] 1 (size - 2)

initToIndex :: Element -> [Int]
initToIndex gen = (:) (-1) $ flipList $ initToPoly gen

initToIndex' :: [Int]
initToIndex' = (-1) : flipList toPoly

flipList :: [Int] -> [Int]
flipList xs = map (indexOf xs) [1..maximum xs]

indexOf :: [Element] -> Element -> Int
indexOf es e = elemIndex' $ elemIndex e es

elemIndex' :: Maybe Int -> Int
elemIndex' Nothing = -1
elemIndex' (Just n) = n

hasBit :: Element -> Int -> Bool
hasBit e i = xor e (2 ^ i) < e

highestBit :: Element -> Int
highestBit e = highestBit' e 1 0 where
               highestBit' e b n | b >= e    = n - 1
                                 | otherwise = highestBit' e (shift b 1) (n + 1)

addPoly :: Poly -> Poly -> Poly
addPoly a b = let (long, short) = if length a > length b
                                  then (a,b)
                                  else (b,a)
                  len           = length short
              in zipWith xor short long ++ drop len long

-- need to draw this out to understand it
multiplyPoly :: Poly -> Poly -> Poly
multiplyPoly [] _ = [0]
multiplyPoly (x:xs) ys = addPoly (map (multiplyElem x) ys) (0 : multiplyPoly xs ys)

-- does not work
dividePoly :: Poly -> Poly -> Poly
dividePoly a b = let diff = degreePoly a - degreePoly b
                     mb   = map (multiplyElem $ last a) b
                 in  if diff == 0
                     then addPoly a mb
                     else dividePoly (addPoly a $ multiplyX mb diff) b

multiplyX :: Poly -> Int -> Poly
multiplyX p n = replicate n 0 ++ p

ddx :: Poly -> Poly
ddx p = tail $ go p 0 [] where
    go [] _ p' = reverse p'
    go (p:ps) 0 p' = go ps 1 (0:p')
    go (p:ps) 1 p' = go ps 0 (p:p')

degreePoly :: Poly -> Int
degreePoly p = f (length p - 1) where
    f n | null p || n == 0 = -1
        | p!!n > 0         = n
        | otherwise        = f $ n - 1

evalPoly :: Poly -> Element -> Element
evalPoly p e = let p' = reverse p
                   eval [] e' = e'
                   eval (p:ps) e' = eval ps (xor (multiplyElem e' e) p)
               in  eval p' 0

-- regular evaluation to test for logic while elemMultiply is not defined
evalPoly' :: Poly -> Element -> Element
evalPoly' p e = let p' = reverse p
                    eval [] e' = e'
                    eval (p:ps) e' = eval ps (e' * e + p)
                in  eval p' 0

multiplyElem :: Element -> Element -> Element
multiplyElem a b = (!!) toPoly $ mod (toIndex!!a + toIndex!!b) (2 ^ m - 1)

invElem :: Element -> Element
invElem e = (!!) toPoly $ 2 ^ m - 1 - toIndex!!e
