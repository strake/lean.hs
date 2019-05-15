module Lean.Raw.C.Decl where

import Foreign
import Foreign.C.Types

import qualified Lean.Raw.C.Decl.Cert as Cert
import Lean.Raw.C.Env (Env)
import Lean.Raw.C.Exception (Exception)
import Lean.Raw.C.Expr (Expr)
import Lean.Raw.C.List (List)
import Lean.Raw.C.Name (Name)
import qualified Lean.Raw.C.Name as Name
import Lean.Wrapper

#include <lean.h>

newtype Kind = Kind CInt
  deriving (Eq)

#{enum Kind, Kind,
  Const = LEAN_DECL_CONST,
  Axiom = LEAN_DECL_AXIOM,
  Def   = LEAN_DECL_DEF,
  Thm   = LEAN_DECL_THM}

data Struct

instance Drop Struct where drop = drop'
foreign import ccall unsafe "&lean_decl_del" drop' :: FunPtr (Ptr Struct -> IO ())

type Decl = Ptr Struct

foreign import ccall unsafe "lean_decl_mk_axiom" mkAxiom :: Name -> List Name.Struct -> Expr -> Ptr Decl -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_decl_mk_const" mkConst :: Name -> List Name.Struct -> Expr -> Ptr Decl -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_decl_mk_def_with" mkDef :: Env -> Name -> List Name.Struct -> Expr -> Expr -> CUInt -> CInt -> Ptr Decl -> Ptr Exception -> IO CInt
foreign import ccall unsafe "lean_decl_check" check :: Env -> Decl -> Ptr Cert.Decl -> Ptr Exception -> IO CInt
