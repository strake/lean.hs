module Lean.Expr (Expr (Lean.Expr.Var, Lean.Expr.Sort)) where

import System.IO.Unsafe

import Lean.Exception ()
import Lean.IO.Util
import qualified Lean.Raw.C.Expr as Raw
import Lean.Wrapper
import Lean.Wrapper.Univ (Univ (Univ))
import qualified Lean.Wrapper.Univ as Univ

newtype Expr = Expr { raw :: Wrapper Raw.Struct }

kindChecked :: Raw.Kind -> (Wrapper Raw.Struct -> IO b) -> Expr -> Maybe b
kindChecked k f = checked ((==) k . Raw.kind) f . raw

pattern Var :: Word -> Expr
pattern Var k <- (kindChecked Raw.Var (liftLean1 Raw.getVarIdx) -> Just k) where
    Var = Expr . unsafePerformIO . liftLean0 . Raw.mkVar . fromIntegral

pattern Sort :: Univ -> Expr
pattern Sort u <- (fmap Univ . kindChecked Raw.Sort (liftLean1 Raw.getSortUniv) -> Just u) where
    Sort = Expr . unsafePerformIO . liftLean1 Raw.mkSort . Univ.raw
