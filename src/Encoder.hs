module Encoder where

import Galois
import Types
import Config

encodeBlock :: Poly -> Poly
encodeBlock msg = let msgShifted = polyMultiplyX msg $ 2 * t
                      remainder  = polyDivide msgShifted codeGenerator
                  in  polyAdd msgShifted remainder
