module Main where

import System.IO
import ReedSolomon

main :: IO ()
main = do
    x <- getLine
    print $ encode x
