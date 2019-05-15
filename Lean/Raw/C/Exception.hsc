module Lean.Raw.C.Exception where

import Foreign
import Foreign.C.Types

import Lean.Wrapper

#include <lean.h>

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_exception_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Exception = Ptr Struct

newtype Kind = Kind CInt
  deriving (Eq)

#{enum Kind, Kind,
  Null        = LEAN_NULL_EXCEPTION,
  System      = LEAN_SYSTEM_EXCEPTION,
  OutOfMemory = LEAN_OUT_OF_MEMORY,
  Interrupted = LEAN_INTERRUPTED,
  Kernel      = LEAN_KERNEL_EXCEPTION,
  Parser      = LEAN_PARSER_EXCEPTION}

foreign import ccall unsafe "lean_exception_get_message" message :: Exception -> IO (Ptr CChar)
foreign import ccall unsafe "lean_exception_get_detailed_message" detailedMessage :: Exception -> IO (Ptr CChar)
foreign import ccall unsafe "lean_exception_get_kind" kind :: Exception -> Kind
