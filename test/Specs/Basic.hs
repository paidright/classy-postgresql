{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Specs.Basic where

import Classy.Postgresql.Control
import Test.Hspec

-- Core.Person defines the Person entity and the relevant sql queries
import Core.Person (Person (..), getPerson, newPerson, renderPerson)

spec :: Spec
spec = describe "Basic usage" $ do
  it "can create and get a person by id" $ do
    p <- mkPoolWithRollback (mkDefaultConfig "host=localhost dbname=classy")

    result <- runDb p $ do
      p <- newPerson "gareth" "gareth.stokes@paidright.io"
      getPerson p.id

    case result of
      Left err -> hPrint stderr $ renderPgError err
      Right a -> hPrint stdout $ renderPerson <$> a

    pure ()
