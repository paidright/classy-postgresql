{-# LANGUAGE NoImplicitPrelude #-}

module Classy.Prelude (
  module X
  , strToText
  , strToBS
  , textToBS
  , textToBS'
  , bsToText
  , bsToText'
  , textToStrict
  , textFromStrict
  , bsToStrict
  , bsFromStrict
) where

import Control.Applicative as X
import Control.Lens as X (set, view)
import Control.Lens.TH as X (makeClassyPrisms)
import Control.Monad as X
import Control.Monad.Except as X (MonadError)
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Control.Monad.Trans.Either as X
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), bool, not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import Data.Char as X (Char)
import Data.Either as X (Either (..), either)
import Data.Foldable as X
import Data.Function as X (const, flip, id, ($), (&), (.))
import Data.Functor as X (($>))
import Data.Int as X
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe)
import Data.Monoid as X (Monoid (..), (<>))
import Data.Text as X (Text)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Data.Traversable as X
import System.IO as X (IO, hPrint, hPutStrLn, stderr, stdin)
import Text.Read as X (Read (..), readMaybe)
import Prelude as X (
  Bounded (..)
  , Double
  , Enum
  , Eq (..)
  , Integral (..)
  , Num (..)
  , Ord (..)
  , Show (..)
  , error
  , fromIntegral
  , fst
  , seq
  , snd
  , undefined
  , (/)
  , (^)
 )
import qualified Prelude

strToText :: Prelude.String -> Text
strToText = Data.Text.pack

strToBS :: Prelude.String -> ByteString
strToBS = Data.ByteString.Char8.pack

bsToText :: ByteString -> Text
bsToText = Data.Text.Encoding.decodeUtf8

textToBS :: Text -> ByteString
textToBS = Data.Text.Encoding.encodeUtf8

bsToText' :: Data.ByteString.Lazy.ByteString -> Data.Text.Lazy.Text
bsToText' = Data.Text.Lazy.Encoding.decodeUtf8

textToBS' :: Data.Text.Lazy.Text -> Data.ByteString.Lazy.ByteString
textToBS' = Data.Text.Lazy.Encoding.encodeUtf8

textToStrict :: Data.Text.Lazy.Text -> Data.Text.Text
textToStrict = Data.Text.Lazy.toStrict

textFromStrict :: Data.Text.Text -> Data.Text.Lazy.Text
textFromStrict = Data.Text.Lazy.fromStrict

bsToStrict :: Data.ByteString.Lazy.ByteString -> Data.ByteString.ByteString
bsToStrict = Data.ByteString.Lazy.toStrict

bsFromStrict :: Data.ByteString.ByteString -> Data.ByteString.Lazy.ByteString
bsFromStrict = Data.ByteString.Lazy.fromStrict
