module Main where

import ReedSolomon

import System.IO

main :: IO ()
main = do
    x <- getLine
    print $ encode x
