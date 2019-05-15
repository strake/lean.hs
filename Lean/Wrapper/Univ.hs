module Lean.Wrapper.Univ where

import qualified Lean.Raw.C.Univ as Raw
import Lean.Wrapper

newtype Univ = Univ { raw :: Wrapper Raw.Struct }
