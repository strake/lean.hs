module Lean.Name where

import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import Foreign.Ptr
import System.IO.Unsafe
import Util

import Lean.Exception ()
import Lean.IO.Util
import qualified Lean.Raw.C.Name as Raw
import Lean.Wrapper

newtype Name = Name { raw :: Wrapper Raw.Struct }

instance Show Name where
    show (Name name) = Text.unpack . unsafePerformIO $ liftLean1 Raw.toString name

instance Eq Name where
    Name a == Name b = (/= 0) . unsafePerformIO $ (Raw.eq `on` unwrap') a b

instance Ord Name where
    Name a <= Name b = ((/= 0) . unsafePerformIO $ (Raw.lt `on` unwrap') a b) || Name a == Name b

{-# COMPLETE Anonymous, Str, Idx #-}

pattern Anonymous :: Name
pattern Anonymous <- ((/= 0) . unsafePerformIO . Raw.isAnonymous . unwrap' . raw -> True) where
    Anonymous = Name . unsafePerformIO $ liftLean0 Raw.mkAnonymous

pattern Str :: Name -> Text -> Name
pattern Str name xs <- (checked ((/= 0) . unsafePerformIO . Raw.isStr) (fmap Name . liftLean1 Raw.getPrefix &=& liftLean1 Raw.getStr) . raw -> Just (name, xs)) where
    Str (Name name) = Name . unsafePerformIO . flip Text.useAsPtr (pure . flip (liftLean1 . flip Raw.mkStr) name . castPtr) . flip Text.snoc '\0'

pattern Idx :: Name -> Word -> Name
pattern Idx name k <- (checked ((/= 0) . unsafePerformIO . Raw.isIdx) (fmap Name . liftLean1 Raw.getPrefix &=& liftLean1 Raw.getIdx) . raw -> Just (name, k)) where
    Idx (Name name) = Name . unsafePerformIO . flip (liftLean1 . flip Raw.mkIdx) name . fromIntegral
