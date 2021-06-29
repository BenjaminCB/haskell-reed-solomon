module Encoder where

import qualified Galois as G
import Types
import qualified Config as C

encodeBlock :: Poly -> Poly
encodeBlock msg = let msgShifted = G.polyMultiplyX msg $ 2 * C.t
                      remainder  = G.polyDivide msgShifted G.codeGenerator
                  in  G.polyAdd msgShifted remainder
