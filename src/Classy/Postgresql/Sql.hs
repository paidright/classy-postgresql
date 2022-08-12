{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Classy.Postgresql.Sql where

import Classy.Postgresql.Control
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import qualified Database.PostgreSQL.Simple as Postgresql

data SqlService m = SqlService
  { query ::
      forall a.
      forall b.
      (ToRow a, FromRow b) =>
      Postgresql.Query ->
      a ->
      m [b]
  , query_ ::
      forall b.
      (FromRow b) =>
      Postgresql.Query ->
      m [b]
  , mandatory ::
      forall a.
      forall b.
      (ToRow a, FromRow b) =>
      Postgresql.Query ->
      a ->
      m b
  , mandatory_ ::
      forall b.
      (FromRow b) =>
      Postgresql.Query ->
      m b
  , unique ::
      forall a.
      forall b.
      (ToRow a, FromRow b) =>
      Postgresql.Query ->
      a ->
      m (Maybe b)
  , unique_ ::
      forall b.
      (FromRow b) =>
      Postgresql.Query ->
      m (Maybe b)
  , execute ::
      forall a.
      (ToRow a) =>
      Postgresql.Query ->
      a ->
      m Int64
  , execute_ ::
      Postgresql.Query ->
      m Int64
  }

-- Implmentation
--

mkSqlService ::
  (MonadIO m, AsPgError e, MonadError e m, MonadReader PgContext m) =>
  SqlService m
mkSqlService =
  SqlService
    { query = \q p -> do
        (PgContext c) <- ask
        safely q (Postgresql.query c q p)
    , query_ = \q -> do
        (PgContext c) <- ask
        safely q (Postgresql.query_ c q)
    , mandatory = \q p -> do
        (PgContext c) <- ask
        definitely q . possibly q $ safely q (Postgresql.query c q p)
    , mandatory_ = \q -> do
        (PgContext c) <- ask
        definitely q . possibly q $ safely q (Postgresql.query_ c q)
    , unique = \q p -> do
        (PgContext c) <- ask
        possibly q $ safely q (Postgresql.query c q p)
    , unique_ = \q -> do
        (PgContext c) <- ask
        possibly q $ safely q (Postgresql.query_ c q)
    , execute = \q p -> do
        (PgContext c) <- ask
        safely q $ Postgresql.execute c q p
    , execute_ = \q -> do
        (PgContext c) <- ask
        safely q $ Postgresql.execute_ c q
    }

-- Helpers
--

possibly ::
  (MonadIO m, AsPgError e, MonadError e m) =>
  Postgresql.Query ->
  m [a] ->
  m (Maybe a)
possibly q db = do
  as <- db
  case as of
    [] ->
      pure Nothing
    [x] ->
      pure . Just $ x
    (_x : _xs) -> throwing _PgTooManyResults (q, length as)

definitely ::
  (MonadIO m, AsPgError e, MonadError e m) =>
  Postgresql.Query ->
  m (Maybe a) ->
  m a
definitely q db = do
  r <- db
  case r of
    Nothing -> throwing _PgNoResults q
    Just a -> pure a
