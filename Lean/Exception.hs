module Lean.Exception
    (Exception, message, detailedMessage, kind,
     Kind (Null, System, OutOfMemory, Interrupted, Kernel, Parser)) where

import qualified Control.Exception.Base as Base
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO.Unsafe

import Lean.IO.Util
import Lean.Raw.C.Exception (Kind)
import qualified Lean.Raw.C.Exception as Raw
import Lean.Wrapper

type Exception = Wrapper Raw.Struct

instance Base.Exception Exception where displayException = Text.unpack . message

message, detailedMessage :: Exception -> Text
message = unsafePerformIO <<< fromRaw <=< Raw.message . unwrap'
detailedMessage = unsafePerformIO <<< fromRaw <=< Raw.detailedMessage . unwrap'

kind :: Exception -> Raw.Kind
kind = Raw.kind . unwrap'
