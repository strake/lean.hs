module Lean.Raw.C.Env where

import Foreign
import Foreign.C.Types

import qualified Lean.Raw.C.Decl.Cert as Cert (Decl)
import Lean.Raw.C.Exception (Exception)
import Lean.Wrapper

#include <lean.h>

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_env_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Env = Ptr Struct

foreign import ccall unsafe "lean_env_mk_std" mkStd :: CUInt -> Ptr Env -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_env_add" add :: Env -> Cert.Decl -> Ptr Env -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_env_replace" replace :: Env -> Cert.Decl -> Ptr Env -> Ptr Exception -> IO CInt
