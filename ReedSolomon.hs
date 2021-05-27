module ReedSolomon where

import Encoder
import DataProcessing
import Types

encode :: String -> [Poly]
encode str = map encodeBlock $ strToPoly str

