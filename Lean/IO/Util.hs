{-# LANGUAGE TypeFamilies #-}

module Lean.IO.Util where

import Control.Applicative
import Control.Exception.Base
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe
import System.IO.Unsafe

import Lean.Wrapper

unwrap' :: Wrapper a -> Ptr a
unwrap' = unsafeForeignPtrToPtr . unwrap

class Storable (Raw a) => FromRaw a where
    type Raw a
    fromRaw :: Raw a -> IO a

instance Drop a => FromRaw (Wrapper a) where
    type Raw (Wrapper a) = Ptr a
    fromRaw = wrap

instance FromRaw Bool where
    type Raw Bool = CInt
    fromRaw = pure . (/= 0)

instance FromRaw Word where
    type Raw Word = CUInt
    fromRaw = pure . fromIntegral

instance FromRaw Int where
    type Raw Int = CInt
    fromRaw = pure . fromIntegral

instance FromRaw Text where
    type Raw Text = Ptr CChar
    fromRaw = getCompose $ Compose (liftA2 (=<<) Text.fromPtr (fmap fromIntegral . cNulArrayLen) . castPtr) <* Compose dropString

foreign import ccall unsafe "lean_string_del" dropString :: Ptr CChar -> IO ()

liftLean0 :: (FromRaw z, Drop ex, Exception (Wrapper ex)) => (Ptr (Raw z) -> Ptr (Ptr ex) -> IO CInt) -> IO z
liftLean0 f = alloca $ \ p -> alloca $ \ q ->
    f p q >>= \ case 0 -> peek q >>= wrap >>= throw; _ -> peek p >>= fromRaw

liftLean1 :: (FromRaw z, Drop ex, Exception (Wrapper ex)) => (Ptr a -> Ptr (Raw z) -> Ptr (Ptr ex) -> IO CInt) -> Wrapper a -> IO z
liftLean1 f a = liftLean0 $ f (unwrap' a)

liftLean2 :: (FromRaw z, Drop ex, Exception (Wrapper ex)) => (Ptr a -> Ptr b -> Ptr (Raw z) -> Ptr (Ptr ex) -> IO CInt) -> Wrapper a -> Wrapper b -> IO z
liftLean2 f a b = liftLean0 $ f (unwrap' a) (unwrap' b)

liftLean3 :: (FromRaw z, Drop ex, Exception (Wrapper ex)) => (Ptr a -> Ptr b -> Ptr c -> Ptr (Raw z) -> Ptr (Ptr ex) -> IO CInt) -> Wrapper a -> Wrapper b -> Wrapper c -> IO z
liftLean3 f a b c = liftLean0 $ f (unwrap' a) (unwrap' b) (unwrap' c)

checked :: (Ptr s -> Bool) -> (Wrapper s -> IO z) -> Wrapper s -> Maybe z
checked pred f s = unsafePerformIO . sequenceA $ f s <$ guard (pred (unwrap' s))

cNulArrayLen :: _ => Ptr a -> IO Word
cNulArrayLen = go 0 where
    go n ptr = peek ptr >>= \ case
        0 -> pure n
        _ -> go (n+1) $ plusPtr ptr 1
