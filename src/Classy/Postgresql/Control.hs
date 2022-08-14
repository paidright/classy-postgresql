{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Classy.Postgresql.Control where

import Control.Exception (Exception, Handler (Handler), catches, throwIO, try)
import Control.Exception.Base (bracket_)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgresql

{-

ClassyPostgresql
----

A lightweight extention to PostgresqlSimple that adds

  * Classy optics + MTL for Postgres
  * A default transaction for every connection
  * Easily manage transactions over multiple queries

-}

data PgError
  = PgSqlError Postgresql.Query Postgresql.SqlError
  | PgQueryError Postgresql.Query Postgresql.QueryError
  | PgFormatError Postgresql.Query Postgresql.FormatError
  | PgResultError Postgresql.Query Postgresql.ResultError
  | PgTooManyResults Postgresql.Query Int
  | PgNoResults Postgresql.Query
  | PgEncodingInvariant Postgresql.Query Text Text
  deriving (Show, Eq)

makeClassyPrisms ''PgError
instance Exception PgError

data RollbackException = RollbackException PgError
  deriving (Eq, Show)

instance Exception RollbackException

renderPgError :: PgError -> Text
renderPgError err =
  case err of
    PgSqlError q e ->
      "Sql error: " <> show e <> ", for query: " <> show q
    PgQueryError q e ->
      "Query error: " <> show e <> ", for query: " <> show q
    PgFormatError q e ->
      "Format error: " <> show e <> ", for query: " <> show q
    PgResultError q e ->
      "Result error: " <> show e <> ", for query: " <> show q
    PgTooManyResults q n ->
      "Too many results: " <> show n <> ", for query: " <> show q
    PgNoResults q ->
      "No results for query: " <> show q
    PgEncodingInvariant q field encoding ->
      "Could not decode: "
        <> "field = '"
        <> show field
        <> "',"
        <> "type = '"
        <> show encoding
        <> "',"
        <> "query = "
        <> show q

data Config = Config
  { connectionString :: !ByteString
  , poolCacheTTL :: !Double
  , poolMaxResources :: !Int
  }

data PgContext = PgContext Postgresql.Connection

newtype ConnectionPool m = ConnectionPool
  { getConnectionPool ::
      forall a.
      (Postgresql.Connection -> m a) ->
      m a
  }

mkDefaultConfig :: ByteString -> Config
mkDefaultConfig cs = Config cs 1 8

poolConfigFrom :: Config -> Pool.PoolConfig Postgresql.Connection
poolConfigFrom config =
  Pool.PoolConfig
    { Pool.createResource =
        Postgresql.connectPostgreSQL config.connectionString
    , Pool.freeResource =
        Postgresql.close
    , Pool.poolCacheTTL =
        config.poolCacheTTL
    , Pool.poolMaxResources =
        config.poolMaxResources
    }

mkPoolWithTx :: Config -> IO (ConnectionPool (EitherT PgError IO))
mkPoolWithTx config =
  mkPoolWith
    (\c -> Postgresql.withTransaction c)
    config

mkPoolWithRollback :: Config -> IO (ConnectionPool (EitherT PgError IO))
mkPoolWithRollback config =
  mkPoolWith
    (\c -> bracket_ (Postgresql.begin c) (Postgresql.rollback c))
    config

mkPoolWith ::
  (forall a. Postgresql.Connection -> IO a -> IO a) ->
  Config ->
  IO (ConnectionPool (EitherT PgError IO))
mkPoolWith transaction config = do
  pool <- Pool.newPool (poolConfigFrom config)
  pure $
    ConnectionPool $ \db -> do
      newEitherT $
        Pool.withResource pool $ \c -> do
          transaction c $ do
            r <- runEitherT $ do
              runReaderT (pure ()) (PgContext c)
              db c
            case r of
              Left e -> do
                throwIO (RollbackException e)
              Right _ -> pure r

runConnectionPool ::
  (MonadIO m, AsPgError e, MonadError e m) =>
  ConnectionPool m ->
  ReaderT PgContext m a ->
  m a
runConnectionPool pool action =
  getConnectionPool pool $ runReaderT action . PgContext

safelyIO ::
  Postgresql.Query ->
  IO a ->
  IO (Either PgError a)
safelyIO query action =
  catches
    (Right <$> action)
    [ Handler $ pure . Left . PgSqlError query
    , Handler $ pure . Left . PgQueryError query
    , Handler $ pure . Left . PgFormatError query
    , Handler $ pure . Left . PgResultError query
    ]

safely ::
  (MonadIO m, AsPgError e, MonadError e m) =>
  Postgresql.Query ->
  IO a ->
  m a
safely query action = do
  r <- liftIO (safelyIO query action)
  case r of
    Right a -> pure a
    Left (PgSqlError q e) -> throwing _PgSqlError (q, e)
    Left (PgQueryError q e) -> throwing _PgQueryError (q, e)
    Left (PgFormatError q e) -> throwing _PgFormatError (q, e)
    Left (PgResultError q e) -> throwing _PgResultError (q, e)
