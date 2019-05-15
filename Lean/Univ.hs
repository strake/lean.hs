module Lean.Univ (Univ (Lean.Univ.Zero, Lean.Univ.Succ, Lean.Univ.Max, Lean.Univ.Imax, Lean.Univ.Param, Lean.Univ.Meta), normalize) where

import Control.Arrow ((***))
import Foreign.Ptr
import System.IO.Unsafe
import Util

import Lean.IO.Util
import Lean.Exception ()
import Lean.Name (Name (Name))
import qualified Lean.Name as Name
import qualified Lean.Raw.C.Univ as Raw
import Lean.Wrapper.Univ

instance FromRaw Univ where
    type Raw Univ = Ptr Raw.Struct
    fromRaw = fmap Univ . fromRaw

instance Eq Univ where
    Univ a == Univ b = unsafePerformIO $ liftLean2 Raw.eq a b

instance Ord Univ where
    Univ a <= Univ b = (unsafePerformIO $ liftLean2 Raw.lt a b) || Univ a == Univ b

{-# COMPLETE Zero, Succ, Max, Imax, Param, Meta #-}

pattern Zero :: Univ
pattern Zero <- (Raw.kind . unwrap' . raw -> Raw.Zero) where
    Zero = Univ . unsafePerformIO $ liftLean0 Raw.mkZero

pattern Succ :: Univ -> Univ
pattern Succ u <- (fmap Univ . checked ((==) Raw.Succ . Raw.kind) (liftLean1 Raw.getPred) . raw -> Just u) where
    Succ = Univ . unsafePerformIO . liftLean1 Raw.mkSucc . raw

pattern Max :: Univ -> Univ -> Univ
pattern Max u v <- (fmap (Univ *** Univ) . checked ((==) Raw.Max . Raw.kind) (liftLean1 Raw.getMaxLhs &=& liftLean1 Raw.getMaxRhs) . raw -> Just (u, v)) where
    Max = curry $ Univ . unsafePerformIO . \ (Univ u, Univ v) -> liftLean2 Raw.mkMax u v

pattern Imax :: Univ -> Univ -> Univ
pattern Imax u v <- (fmap (Univ *** Univ) . checked ((==) Raw.Imax . Raw.kind) (liftLean1 Raw.getMaxLhs &=& liftLean1 Raw.getMaxRhs) . raw -> Just (u, v)) where
    Imax = curry $ Univ . unsafePerformIO . \ (Univ u, Univ v) -> liftLean2 Raw.mkImax u v

pattern Param :: Name -> Univ
pattern Param name <- (fmap Name . checked ((==) Raw.Param . Raw.kind) (liftLean1 Raw.getName) . raw -> Just name) where
    Param = Univ . unsafePerformIO . liftLean1 Raw.mkParam . Name.raw

pattern Meta :: Name -> Univ
pattern Meta name <- (fmap Name . checked ((==) Raw.Meta . Raw.kind) (liftLean1 Raw.getName) . raw -> Just name) where
    Meta = Univ . unsafePerformIO . liftLean1 Raw.mkMeta . Name.raw

normalize :: Univ -> Univ
normalize = unsafePerformIO . liftLean1 Raw.normalize . raw
