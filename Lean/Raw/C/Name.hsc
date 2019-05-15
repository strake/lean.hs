module Lean.Raw.C.Name where

import Foreign
import Foreign.C.Types

import Lean.Raw.C.Exception (Exception)
import Lean.Raw.C.List (Elemental, List)
import qualified Lean.Raw.C.List as List
import Lean.Wrapper

#include <lean.h>

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_name_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Name = Ptr Struct

foreign import ccall unsafe "lean_name_del" drop :: Name -> IO ()
foreign import ccall unsafe "lean_name_mk_anonymous" mkAnonymous :: Ptr Name -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_name_mk_str" mkStr :: Name -> Ptr CChar -> Ptr Name -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_name_mk_idx" mkIdx :: Name -> CUInt -> Ptr Name -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_name_eq" eq :: Name -> Name -> IO CInt
foreign import ccall unsafe "lean_name_lt" lt :: Name -> Name -> IO CInt
foreign import ccall unsafe "lean_name_to_string" toString :: Name -> Ptr (Ptr CChar) -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_name_is_anonymous" isAnonymous :: Name -> IO CInt
foreign import ccall unsafe "lean_name_is_str" isStr :: Name -> IO CInt
foreign import ccall unsafe "lean_name_is_idx" isIdx :: Name -> IO CInt
foreign import ccall unsafe "lean_name_get_prefix" getPrefix :: Name -> Ptr Name -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_name_get_str" getStr :: Name -> Ptr (Ptr CChar) -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_name_get_idx" getIdx :: Name -> Ptr CUInt -> Ptr Exception -> IO CInt

instance Elemental Struct where
    data Struct Struct
    mkNil = mkNil
    mkCons = mkCons
    head = head
    tail = tail
    isCons = isCons

foreign import ccall unsafe "lean_list_name_del" dropList :: List Struct -> IO ()
foreign import ccall unsafe "lean_list_name_mk_nil" mkNil :: Ptr (List Struct) -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_list_name_mk_cons" mkCons :: Name -> List Struct -> Ptr (List Struct) -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_list_name_is_cons" isCons :: List Struct -> IO CInt
foreign import ccall unsafe "lean_list_name_head" head :: List Struct -> Ptr Name -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_list_name_tail" tail :: List Struct -> Ptr (List Struct) -> Ptr Exception -> IO CInt
