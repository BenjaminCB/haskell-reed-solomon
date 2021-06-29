module Main where

import qualified ReedSolomon as RS
import qualified ErrorInjection as EI
import qualified Config as C

import System.IO

main :: IO ()
main = do
    line <- getLine
    let encoded = RS.encode line
    print encoded
    let err = EI.polyBurstErrorInjection encoded C.t
    print err
    let decoded = RS.decode err
    print decoded
