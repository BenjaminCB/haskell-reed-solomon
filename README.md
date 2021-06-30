# Haskell Reed Solomon
This repository contains a very simple Reed Solomon error correction implementation in Haskell. Which is pretty configurable and is mostly intended to be used as a library, thought there is a simple executable as well.

There is also some simple error injection, which is mostly used for testing, the decoding.

## Library
Is of now the library is pretty simple only containing the functions

```
-- Poly = [Int]
encode :: String -> Poly
docede :: Poly -> String
```

## Executable
Using `stack build` an executable `reedSolomon` will be compiled. Once you run the program it will wait for an stdin input. When that it is received the encoded polynomial will be calculated and written to stdout, then an errors are injected and written, lastly the attempted decoding will be calculated and written.

# Configuration
At the moment the Reed Solomon configuration is done within the file `Config.hs`, at the moment there are no parsing of the inputs, so it will try its best even if the input is not a valid Reed Solomon coding.

```
m = 8 :: Int                -- Symbol size
n = 255 :: Int              -- Block size
k = 239 :: Int              -- Message size
t = div (n - k) 2 :: Int    -- Number of correctable symbols
g = 285 :: Int              -- Field generator
```

# Road Map
- Make more top level function for the library, as the are more useful functions than `encode` and `decode`, a lot of the underlying structure is already there, so this should be pretty simple.
- Get rid of `Config.hs` and instead initialize top level functions with currying.
- Parsing of Reed Solomon values to make sure than everything works correctly.
