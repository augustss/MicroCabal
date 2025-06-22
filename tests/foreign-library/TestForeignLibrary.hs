module TestForeignLibrary where

import Foreign.C (CInt)

foreign export ccall myFun :: CInt -> CInt

myFun x = x * 2
