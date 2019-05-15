module Lean.Raw.C.Decl.Cert where

import Foreign

import Lean.Wrapper

#include <lean.h>

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_cert_decl_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Decl = Ptr Struct
