{-# LANGUAGE PackageImports #-}

module Prelude (module A) where

import "base" Prelude as A hiding (drop, id, head, tail, (.))
import Control.Category as A
import Data.Functor.Compose as A
