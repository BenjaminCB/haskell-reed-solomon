module Decoder
    ( decodeBlock
    , syndromes
    , berlekamp
    , forney
    , errorPoly )
    where

import Types
import qualified Config as C
import qualified Galois as G
import qualified Util as U

import Data.Bits

decodeBlock :: Poly -> Poly
decodeBlock received =
    if any (/=0) sp
    then drop (2 * C.t) recovered
    else drop (2 * C.t) received
    where
        sp = syndromes received
        elp = berlekamp sp
        errps = chien elp
        errvs = forney sp elp errps
        errp = errorPoly errps errvs
        recovered = G.polyAdd received errp


syndromes :: Poly -> Poly
syndromes received = calc 0 [] where
    calc i syn | i == 2 * C.t = reverse syn
               | otherwise  = calc (i + 1) (G.polyEval received (G.toPoly!!i) : syn)

berlekamp :: Poly -> Poly
berlekamp sp = berlekamp' 1 0 [1] [0, 1] where
    berlekamp' k l elp c
        | k > 2 * C.t = elp
        | otherwise = let err  = calculateError sp elp k l
                          elp' = G.polyAdd elp $ map (G.elemMultiply err) c
                      in  if 2 * l < k && err /= 0
                          then berlekamp' (k + 1)
                                          (k - l)
                                          elp'
                                          (0 : map (G.elemMultiply $ G.elemInv err) elp)
                          else berlekamp' (k + 1)
                                          l
                                          elp'
                                          (0 : c)

calculateError :: Poly -> Poly -> Int -> Int -> Element
calculateError sp elp k l = calculateError' 1 (sp!!(k - 1)) where
    calculateError' i err
        | i > l     = err
        | otherwise = calculateError' (i + 1)
                                      (xor err $ G.elemMultiply (elp!!i)
                                                              (sp!!(k - 1 - i)))

-- NOT WORKING
-- there might need to be a check to see if the remainder is less than t
-- but i do not know what to return in that case
euclidean :: Poly -> (Poly, Poly)
euclidean sp =
    let xs = replicate (2 * C.t) 0 ++ [1]
        remainder = G.polyDivide xs sp
    in  euclidean' sp remainder

-- returns (error locator, error magnitude
euclidean' :: Poly -> Poly -> (Poly, Poly)
euclidean' dividend divisor
    | G.polyDegree divisor < C.t = (dividend, divisor)
    | otherwise = euclidean' divisor (G.polyDivide dividend divisor)

-- dont know if you can just sum the terms for the first evaluation
-- returns a list of the error positions given the error locater
chien :: Poly -> [Element]
chien = chien' [] 0 where
    chien' rs i ts
        | i == 2 ^ C.m      = rs
        | G.polySum ts == 0 = chien' (2 ^ C.m - 1 - i : rs) (i + 1) (updateChien ts)
        | otherwise       = chien' rs (i + 1) (updateChien ts)

updateChien :: Poly -> Poly
updateChien ts = updateChien' (take 1 ts) 1 (G.toPoly!!1) where
    updateChien' uTs i alpha
        | i == length ts = reverse uTs
        | otherwise = updateChien' (G.elemMultiply alpha (ts!!i) : uTs)
                                   (i + 1)
                                   (G.elemMultiply alpha $ G.toPoly!!1)

forney :: Poly
       -> Poly
       -> [Element]
       -> [Element]
forney sp elp errps = forney' [] 0 where
    elp'    = G.polyDerivative elp
    errMagP = G.polyDivide (G.polyMultiply sp elp) (replicate (2 * C.t) 0 ++ [1])
    xs      = map (G.toPoly!!) errps
    xsInv   = map G.elemInv xs
    forney' errvs i
        | i == length errps = reverse errvs
        | otherwise = forney' (errv : errvs) (i + 1) where
            errv    = G.elemMultiply (xs!!i)
                                   (G.elemMultiply (G.polyEval errMagP (xsInv!!i))
                                                 (G.elemInv (G.polyEval elp' (xsInv!!i))))

errorPoly :: [Element] -> [Element] -> Poly
errorPoly errps errvs = U.insertAll errvs errps $ replicate C.n 0
-- errorPoly errps errvs = errorPoly' (replicate C.n 0) 0 where
--     errorPoly' errp i
--         | i == length errps = errp
--         | otherwise = errorPoly' (insert' i errp) (i + 1)
--     insert' i = U.insert (errvs!!i) (errps!!i)
