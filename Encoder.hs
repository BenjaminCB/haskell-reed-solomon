module Encoder where

import Galois

encodeBlock :: [Element] -> [Element]
encodeBlock msg = let msgShifted = polyMultiplyX msg $ 2 * t
                      remainder  = polyDivide msgShifted codeGenerator
                  in  polyAdd msgShifted remainder
