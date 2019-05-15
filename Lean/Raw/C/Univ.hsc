module Lean.Raw.C.Univ where

import Foreign
import Foreign.C.Types

import Lean.Raw.C.Exception (Exception)
import Lean.Raw.C.Name (Name)
import Lean.Wrapper

#include <lean.h>

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_univ_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Univ = Ptr Struct

foreign import ccall unsafe "lean_univ_mk_zero" mkZero :: Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_mk_succ" mkSucc :: Univ -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_mk_max"  mkMax  :: Univ -> Univ -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_mk_imax" mkImax :: Univ -> Univ -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_mk_param" mkParam :: Name -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_mk_meta" mkMeta :: Name -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_eq" eq :: Univ -> Univ -> Ptr CInt -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_lt" lt :: Univ -> Univ -> Ptr CInt -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_get_pred" getPred :: Univ -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_get_max_lhs" getMaxLhs :: Univ -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_get_max_rhs" getMaxRhs :: Univ -> Ptr Univ -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_get_name" getName :: Univ -> Ptr Name -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_univ_normalize" normalize :: Univ -> Ptr Univ -> Ptr Exception -> IO CInt

newtype Kind = Kind CInt
  deriving (Eq)

#{enum Kind, Kind,
  Zero  = LEAN_UNIV_ZERO,
  Succ  = LEAN_UNIV_SUCC,
  Max   = LEAN_UNIV_MAX,
  Imax  = LEAN_UNIV_IMAX,
  Param = LEAN_UNIV_PARAM,
  Meta  = LEAN_UNIV_META}

foreign import ccall unsafe "lean_univ_kind" kind :: Univ -> Kind
