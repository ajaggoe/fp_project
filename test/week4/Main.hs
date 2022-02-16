{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Main where

import           Control.Monad
import           Control.DeepSeq
import           Data.List
import           GHC.Generics (Generic, Generic1)
import           System.Exit
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Jq.Filters                            as Filter

deriving instance Generic Filter
deriving instance NFData Filter

instance Arbitrary Filter where
    arbitrary = do
        id  <- arbitrary :: Gen String
        f   <- arbitrary
        
        elements [ filterIdentitySC, filterIndexingSC id, filterPipeSC f f, filterCommaSC f f ]

main = defaultMain tests

prop_computes_identity      = total $ filterIdentitySC
prop_computes_indexing id   = total $ filterIndexingSC id
prop_computes_pipe f f      = total $ filterPipeSC f f
prop_computes_comma f f     = total $ filterCommaSC f f

prop_identity_refl          = filterIdentitySC  == filterIdentitySC
prop_indexing_refl          = filterIndexingSC  == filterIndexingSC
prop_pipe_refl              = filterPipeSC      == filterPipeSC
prop_comma_refl             = filterCommaSC     == filterCommaSC

tests = [
    testGroup "Constructors are defined" [
        testProperty "Constructor for identity computes" prop_computes_identity,
        testProperty "Constructor for indexing computes" prop_computes_indexing,
        testProperty "Constructor for pipe computes" prop_computes_pipe,
        testProperty "Constructor for comma computes" prop_computes_comma
    ],
    testGroup "Reflection instances" [
        testProperty "Reflection identity" prop_identity_refl,
        testProperty "Reflection indexing" prop_indexing_refl,
        testProperty "Reflection pipe" prop_pipe_refl,
        testProperty "Reflection comma" prop_comma_refl
    ]
]