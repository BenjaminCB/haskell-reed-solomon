module Decoder where

import Types
import Config
import Galois
import Data.Bits

decodeBlock :: Poly -> Poly
decodeBlock = undefined

syndromes :: Poly -> Poly
syndromes received = calc 0 [] where
    calc i syn | i == 2 * t = reverse syn
               | otherwise  = calc (i + 1) (polyEval received (toPoly!!i) : syn)

berlekamp :: Poly -> Poly
berlekamp syns = berlekamp' 1 0 [1] [0, 1] where
    berlekamp' k l elp c
        | k > 2 * t = elp
        | otherwise = let err  = calculateError syns elp k l
                          elp' = polyAdd elp $ map (elemMultiply err) c
                      in  if 2 * l < k && err /= 0
                          then berlekamp' (k + 1)
                                          (k - l)
                                          elp'
                                          (0 : map (elemMultiply $ elemInv err) elp)
                          else berlekamp' (k + 1)
                                          l
                                          elp'
                                          (0 : c)

calculateError :: Poly -> Poly -> Int -> Int -> Element
calculateError syns elp k l = calculateError' 1 (syns!!(k - 1)) where
    calculateError' index err
        | index > l = err
        | otherwise = calculateError' (index + 1)
                                      (xor err $ elemMultiply (elp!!index)
                                                              (syns!!(k - 1 - index)))

-- there might need to be a check to see if the remainder is less than t
-- but i do not know what to return in that case
euclidean :: Poly -> (Poly, Poly)
euclidean syn =
    let xs = replicate (2 * t) 0 ++ [1]
        remainder = polyDivide xs syn
    in  euclidean' syn remainder

-- returns (error locator, error magnitude
euclidean' :: Poly -> Poly -> (Poly, Poly)
euclidean' dividend divisor
    | polyDegree divisor < t = (dividend, divisor)
    | otherwise = euclidean' (divisor) (polyDivide dividend divisor)

chien :: Poly -> [Element]
chien = undefined

errorPositions :: [Element] -> [Int]
errorPositions = undefined

forney :: Poly
       -> Poly
       -> [Element]
       -> [Element]
forney = undefined

errorPoly :: [Int] -> [Element] -> Poly
errorPoly = undefined
