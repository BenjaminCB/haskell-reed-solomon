module ReedSolomon where

import Encoder
import Decoder
import DataProcessing
import Types

encode :: String -> [Poly]
encode str = map encodeBlock $ strToPoly str

decode :: [Poly] -> String
decode ps = polyToStr $ map decodeBlock ps
