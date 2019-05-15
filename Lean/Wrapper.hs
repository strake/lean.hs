module Lean.Wrapper where

import Foreign

class Drop a where
    drop :: FunPtr (Ptr a -> IO ())

newtype Wrapper a = Wrapper { unwrap :: ForeignPtr a }
  deriving (Show)

wrap :: Drop a => Ptr a -> IO (Wrapper a)
wrap = fmap Wrapper . newForeignPtr drop
