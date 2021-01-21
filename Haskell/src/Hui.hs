module Hui where

import Foreign.C.Types (CInt (CInt))

add :: CInt -> CInt -> IO CInt
add a b = pure $ a + b

foreign export ccall add :: CInt -> CInt -> IO CInt
