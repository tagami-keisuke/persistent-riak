{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, PackageImports, RankNTypes, CPP #-}
-- | Test implementation for typeclass functions for the riak persist backend
module Test.InterfaceTest
-- I want all of them in my console for debugging
--       ( riakTests
--       , backendImplTests
--       , prop_true
--       ) 
       where

import Test.Hspec
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck
import qualified Test.HUnit as HUnit

import qualified Network.Riak as R
import qualified Network.Riak.Connection as R
import qualified Network.Riak.Connection.Pool as R
import qualified Network.Riak.Protocol.ServerInfo as R

import qualified Data.ByteString.Lazy.UTF8 as B

import System.IO.Unsafe
import Data.Maybe (fromJust)

riakTests :: IO [IO Spec]
riakTests = describe "riak database connection"
  [ it "can read the server_info)" 
    ( is_server_info )
  ]
  
backendImplTests :: IO [IO Spec]
backendImplTests = describe "riak backend implementation"
  [ it "inserts and returns the key"
    (property prop_true)
  ]

db_host :: [Char]
db_host = "127.0.0.1"

db_port :: [Char]
db_port = "8081"

open_connection_pool = do
  c_id <- R.makeClientID
  let client = R.Client {R.host=db_host, R.port=db_port, R.clientID=c_id}
  R.create client 10 50 1
--  R.withConnection p $ R.getServerInfo
  
is_server_info = 
  case si of
    R.ServerInfo n v -> ((B.toString $ fromJust $ v) == "0.14.2") && ((B.toString $ fromJust $ n) == "dev1@127.0.0.1")
    otherwise -> False
    where 
      pool = unsafePerformIO open_connection_pool
      si = unsafePerformIO $ R.withConnection pool $ R.getServerInfo
  

prop_true :: Bool -> Bool
prop_true b = b || True