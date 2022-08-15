{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Person where

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
  Text ->
  Text ->
  DbStatement Person
newPerson name email = mandatory mkSqlService sql (name, email)
  where
    sql =
      "insert into person (name, email) \
      \ values (?, ?) \
      \ returning id, name, email"

getPerson ::
  Int64 ->
  DbStatement (Maybe Person)
getPerson pid = unique mkSqlService sql (Only pid)
  where
    sql = "select id, name, email from person where id = ?"
