module Main where

import Control.Monad
import Data.List
import System.Exit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Jq.Json as Json

main = defaultMain tests

null_show :: Bool
null_show = show (jsonNullSC) == "null"

null_eq :: Bool
null_eq = jsonNullSC == jsonNullSC

number_eq_refl :: Int -> Property
number_eq_refl x = (jsonNumberSC x) === (jsonNumberSC x)

tests = [
  testGroup "Show instances" [
    testProperty "show for Null" null_show
    ],
  testGroup "Eq instances" [
    testProperty "eq for Null" null_eq,
    testProperty "eq for Number" number_eq_refl
    ]
  ]
