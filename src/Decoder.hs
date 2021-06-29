module Decoder where

import Types
import Config
import Galois
import Util

import Data.Bits

decodeBlock :: Poly -> Poly
decodeBlock received =
    if any (/=0) sp
    then drop (2 * t) recovered
    else drop (2 * t) received
    where
        sp = syndromes received
        elp = berlekamp sp
        errps = chien elp
        errvs = forney sp elp errps
        errp = errorPoly errps errvs
        recovered = polyAdd received errp


syndromes :: Poly -> Poly
syndromes received = calc 0 [] where
    calc i syn | i == 2 * t = reverse syn
               | otherwise  = calc (i + 1) (polyEval received (toPoly!!i) : syn)

berlekamp :: Poly -> Poly
berlekamp sp = berlekamp' 1 0 [1] [0, 1] where
    berlekamp' k l elp c
        | k > 2 * t = elp
        | otherwise = let err  = calculateError sp elp k l
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
calculateError sp elp k l = calculateError' 1 (sp!!(k - 1)) where
    calculateError' i err
        | i > l     = err
        | otherwise = calculateError' (i + 1)
                                      (xor err $ elemMultiply (elp!!i)
                                                              (sp!!(k - 1 - i)))

-- NOT WORKING
-- there might need to be a check to see if the remainder is less than t
-- but i do not know what to return in that case
euclidean :: Poly -> (Poly, Poly)
euclidean sp =
    let xs = replicate (2 * t) 0 ++ [1]
        remainder = polyDivide xs sp
    in  euclidean' sp remainder

-- returns (error locator, error magnitude
euclidean' :: Poly -> Poly -> (Poly, Poly)
euclidean' dividend divisor
    | polyDegree divisor < t = (dividend, divisor)
    | otherwise = euclidean' (divisor) (polyDivide dividend divisor)

-- dont know if you can just sum the terms for the first evaluation
-- returns a list of the error positions given the error locater
chien :: Poly -> [Element]
chien elp = chien' [] 0 elp where
    chien' rs i ts
        | i == 2 ^ m      = rs
        | polySum ts == 0 = chien' (2^m - 1 - i : rs) (i + 1) (updateChien ts)
        | otherwise       = chien' rs (i + 1) (updateChien ts)

updateChien :: Poly -> Poly
updateChien ts = updateChien' (take 1 ts) 1 (toPoly!!1) where
    updateChien' uTs i alpha
        | i == length ts = reverse uTs
        | otherwise = updateChien' (elemMultiply alpha (ts!!i) : uTs)
                                   (i + 1)
                                   (elemMultiply alpha $ toPoly!!1)

forney :: Poly
       -> Poly
       -> [Element]
       -> [Element]
forney sp elp errps = forney' [] 0 where
    elp'    = polyDerivative elp
    errMagP = polyDivide (polyMultiply sp elp) (replicate (2 * t) 0 ++ [1])
    xs      = map (toPoly!!) errps
    xsInv   = map elemInv xs
    forney' errvs i
        | i == length errps = reverse errvs
        | otherwise = forney' (errv : errvs) (i + 1) where
            errv    = elemMultiply (xs!!i)
                                   (elemMultiply (polyEval errMagP (xsInv!!i))
                                                 (elemInv (polyEval elp' (xsInv!!i))))

errorPoly :: [Element] -> [Element] -> Poly
errorPoly errps errvs = errorPoly' (replicate n 0) 0 where
    errorPoly' errp i
        | i == length errps = errp
        | otherwise = errorPoly' (insert' i errp) (i + 1)
    insert' i = insert (errvs!!i) (errps!!i)
