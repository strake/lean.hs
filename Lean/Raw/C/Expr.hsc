module Lean.Raw.C.Expr where

import Foreign
import Foreign.C.Types

import Lean.Raw.C.Exception (Exception)
import Lean.Raw.C.List (List)
import Lean.Raw.C.Name (Name)
import Lean.Raw.C.Univ (Univ)
import Lean.Wrapper

#include <lean.h>

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_expr_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Expr = Ptr Struct

newtype Kind = Kind CInt
  deriving (Eq)

#{enum Kind, Kind,
  Var    = LEAN_EXPR_VAR,
  Sort   = LEAN_EXPR_SORT,
  Const  = LEAN_EXPR_CONST,
  Local  = LEAN_EXPR_LOCAL,
  Meta   = LEAN_EXPR_META,
  App    = LEAN_EXPR_APP,
  Lambda = LEAN_EXPR_LAMBDA,
  Pi     = LEAN_EXPR_PI,
  Let    = LEAN_EXPR_LET,
  Macro  = LEAN_EXPR_MACRO}

foreign import ccall unsafe "lean_expr_mk_var" mkVar :: CUInt -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_const" mkConst :: Name -> List Univ -> Ptr Expr -> Ptr Exception -> IO CInt
{-
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_mk_sort" mkSort :: Univ -> Ptr Expr -> Ptr Exception -> IO CInt
-}

foreign import ccall unsafe "lean_expr_get_var_idx" getVarIdx :: Expr -> Ptr CUInt -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_expr_get_sort_univ" getSortUniv :: Expr -> Ptr Univ -> Ptr Exception -> IO CInt

foreign import ccall unsafe "lean_expr_kind" kind :: Expr -> Kind
