module Decoder where

import Types
import Config
import Galois

decodeBlock :: Poly -> Poly
decodeBlock = undefined

syndromes :: Poly -> Poly
syndromes received = calc 0 [] where
    calc i syn | i == 2 * t = reverse syn
               | otherwise  = calc (i + 1) (polyEval received (toPoly!!i) : syn)

berlekamp :: Poly -> Poly
berlekamp = undefined

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
