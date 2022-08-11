{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Classy.Postgresql (mkConnectionPool) where

import Data.Pool (Pool)
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
data Error
  = PgSqlError Postgresql.Query Postgresql.SqlError
  | PgQueryError Postgresql.Query Postgresql.QueryError
  | PgFormatError Postgresql.Query Postgresql.FormatError
  | PgResultError Postgresql.Query Postgresql.ResultError
  | PgTooManyResults Postgresql.Query Int
  | PgNoResults Postgresql.Query
  | PgEncodingInvariant Postgresql.Query Text Text
  deriving (Show, Eq)

makeClassyPrisms ''Error

data Config = Config {poolCacheTTL :: !Double, poolMaxResources :: !Int}

runConnectionPool ::
  (MonadIO m, AsError e, MonadError e m) =>
  (Pool Postgresql.Connection) ->
  (Postgresql.Connection -> IO a) ->
  m a
runConnectionPool pool f = liftIO $ Pool.withResource pool f

mkConfig :: Config
mkConfig = Config 1 1

mkConnectionPool :: ByteString -> Config -> IO (Pool Postgresql.Connection)
mkConnectionPool connectionString config = do
  let c =
        Pool.PoolConfig
          { Pool.createResource =
              Postgresql.connectPostgreSQL connectionString
          , Pool.freeResource =
              Postgresql.close
          , Pool.poolCacheTTL =
              config.poolCacheTTL
          , Pool.poolMaxResources =
              config.poolMaxResources
          }
  Pool.newPool c
