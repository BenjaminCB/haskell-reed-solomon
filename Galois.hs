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
toIndex = initToIndex' toPoly


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
initToIndex gen = (:) (-1) $ listFlip $ initToPoly gen

initToIndex' :: [Element] -> [Int]
initToIndex' xs = (-1) : listFlip xs

initCodeGenrator :: [Element] -> [Element]
initCodeGenrator xs = init [head toPoly, 1] 1 where
    init code n | n < 2 * t = init (polyMultiply code [toPoly!!n, 1]) (n + 1)
                | otherwise = code

listFlip :: [Int] -> [Int]
listFlip xs = map (indexOf xs) [1..maximum xs]

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

polyAdd :: Poly -> Poly -> Poly
polyAdd a b = let (long, short) = if length a > length b
                                  then (a,b)
                                  else (b,a)
                  len           = length short
              in zipWith xor short long ++ drop len long

-- need to draw this out to understand it
polyMultiply :: Poly -> Poly -> Poly
polyMultiply [] _ = [0]
polyMultiply (x:xs) ys = polyAdd (map (elemMultiply x) ys) (0 : polyMultiply xs ys)

polyDivide :: Poly -> Poly -> Poly
polyDivide a b = let diff   = polyDegree a - polyDegree b
                     aOverB = elemMultiply (last a) (elemInv $ last b)
                     mb     = map (elemMultiply aOverB) b
                     ta     = take (length a - 1)
                 in  if diff == 0
                     then ta $ polyAdd a mb
                     else polyDivide (ta $ polyAdd a $ polyMultiplyX mb diff) b

polyMultiplyX :: Poly -> Int -> Poly
polyMultiplyX p n = replicate n 0 ++ p

ddx :: Poly -> Poly
ddx p = tail $ go p 0 [] where
    go [] _ p' = reverse p'
    go (p:ps) 0 p' = go ps 1 (0:p')
    go (p:ps) 1 p' = go ps 0 (p:p')

polyDegree :: Poly -> Int
polyDegree p = f (length p - 1) where
    f n | null p || n == 0 = -1
        | p!!n > 0         = n
        | otherwise        = f $ n - 1

polyEval :: Poly -> Element -> Element
polyEval p e = let p' = reverse p
                   eval [] e' = e'
                   eval (p:ps) e' = eval ps (xor (elemMultiply e' e) p)
               in  eval p' 0

-- regular evaluation to test for logic while elemMultiply is not defined
polyEval' :: Poly -> Element -> Element
polyEval' p e = let p' = reverse p
                    eval [] e' = e'
                    eval (p:ps) e' = eval ps (e' * e + p)
                in  eval p' 0

elemMultiply :: Element -> Element -> Element
elemMultiply a b = (!!) toPoly $ mod (toIndex!!a + toIndex!!b) (2 ^ m - 1)

elemInv :: Element -> Element
elemInv e = (!!) toPoly $ mod (nElements - (toIndex!!e)) nElements where
    nElements = 2 ^ m - 1
