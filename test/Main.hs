{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classy.Postgresql.Control
import Classy.Postgresql.Sql
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import qualified Database.PostgreSQL.Simple as Postgresql
import GHC.Generics (Generic)

data Person = Person
  { id :: Int64
  , name :: Text
  , email :: Text
  }
  deriving (Eq, Ord, Show, Generic, ToRow, FromRow)

renderPerson :: Person -> Text
renderPerson = name

newPerson ::
  (MonadIO m, AsPgError e, MonadError e m, MonadReader PgContext m) =>
  Text ->
  Text ->
  m Person
newPerson name email = mandatory mkSqlService sql (name, email)
  where
    sql =
      "insert into person (name, email) \
      \ values (?, ?) \
      \ returning id, name, email"

getPerson ::
  (MonadIO m, AsPgError e, MonadError e m, MonadReader PgContext m) =>
  Int64 ->
  m (Maybe Person)
getPerson pid = unique mkSqlService sql (Only pid)
  where
    sql = "select id, name, email from person where id = ?"

main :: IO ()
main = do
  p <- mkPoolWithRollback (mkDefaultConfig "host=localhost dbname=classy")

  result <- runEitherT $
    runConnectionPool p $ do
      p <- newPerson "gareth" "gareth.stokes@paidright.io"
      getPerson p.id

  case result of
    Left err -> hPrint stderr $ renderPgError err
    Right a -> hPrint stdout $ renderPerson <$> a

  pure ()
