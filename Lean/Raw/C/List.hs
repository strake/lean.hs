module Lean.Raw.C.List where

import Foreign
import Foreign.C.Types
import Lean.Raw.C.Exception (Exception)

type List a = Ptr (Struct a)

class Elemental a where
    data Struct a
    mkNil :: Ptr (List a) -> Ptr Exception -> IO CInt
    mkCons :: Ptr a -> List a -> Ptr (List a) -> Ptr Exception -> IO CInt
    head :: List a -> Ptr (Ptr a) -> Ptr Exception -> IO CInt
    tail :: List a -> Ptr (List a) -> Ptr Exception -> IO CInt
    isCons :: List a -> IO CInt
