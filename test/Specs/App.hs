{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Specs.App where

import Classy.Postgresql.Control
import Classy.Postgresql.Sql
import Core.Person
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import qualified Database.PostgreSQL.Simple as Postgresql
import GHC.Generics (Generic)
import Test.Hspec

--

data ApplicationError
  = GenericDatabaseError PgError
  | NonDatabaseError

makeClassyPrisms ''ApplicationError

instance AsPgError ApplicationError where
  _PgError = _GenericDatabaseError

renderApplicationError :: ApplicationError -> Text
renderApplicationError e =
  case e of
    GenericDatabaseError err -> renderPgError err
    NonDatabaseError -> "non-db related error"

db ::
  (MonadIO m, AsApplicationError e, MonadError e m) =>
  ConnectionPool ->
  DbStatement a ->
  m a
db pool action = do
  result <- liftIO $ (runDb pool action)
  case result of
    Right a -> pure a
    Left err -> throwing _GenericDatabaseError err

--

spec :: Spec
spec = describe "Application usage" $ do
  it "can compose error messages with domain logic" $ do
    p <- mkPoolWithRollback (mkDefaultConfig "host=localhost dbname=classy")

    result <- runEitherT $
      db p $ do
        p <- newPerson "gareth" "gareth.stokes@paidright.io"
        getPerson p.id

    case result of
      Left err -> hPrint stderr $ renderApplicationError err
      Right a -> hPrint stdout $ renderPerson <$> a

    pure ()
