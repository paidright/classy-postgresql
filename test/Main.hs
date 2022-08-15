module Main where

import Test.Hspec

import qualified Specs.App as App
import qualified Specs.Basic as Basic

main :: IO ()
main = hspec $ do
  Basic.spec
  App.spec
