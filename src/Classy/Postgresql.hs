{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Classy.Postgresql (mkConnectionPool) where

import Classy.Prelude
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgresql

{-

ClassyPostgresql
----

A lightweight extention to PostgresqlSimple that adds

  * Classy optics + MTL for Postgres
  * A default transaction for every connection
  * Easily manage transactions over multiple connections

-}
data PostgresqlError
  = PgSqlError Postgresql.Query Postgresql.SqlError
  | PgQueryError Postgresql.Query Postgresql.QueryError
  | PgFormatError Postgresql.Query Postgresql.FormatError
  | PgResultError Postgresql.Query Postgresql.ResultError
  | PgTooManyResults Postgresql.Query Int
  | PgNoResults Postgresql.Query
  | PgEncodingInvariant Postgresql.Query Text Text
  deriving (Show, Eq)

makeClassyPrisms ''PostgresqlError

data ConnectionPool = ConnectionPool

runConnectionPool ::
  (MonadIO m, AsPostgresqlError e, MonadError e m) =>
  ConnectionPool ->
  (Postgresql.Connection -> m a) ->
  m a
runConnectionPool = undefined

mkConnectionPool :: ByteString -> undefined -> IO ConnectionPool
mkConnectionPool connectionString config = do
  let c =
        Pool.PoolConfig
          { Pool.createResource =
              Postgresql.connectPostgreSQL connectionString
          , Pool.freeResource =
              Postgresql.close
          , Pool.poolCacheTTL =
              1
          , Pool.poolMaxResources = 1
          }
  pool <- Pool.newPool c
  undefined
