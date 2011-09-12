-- | Main module for the test runner of the riak persist backend
module Main (main) where

import Test.Hspec

import Test.InterfaceTest

main = hspecX $ descriptions $
        [ riakTests
        , backendImplTests
        , jsonTests
        ]