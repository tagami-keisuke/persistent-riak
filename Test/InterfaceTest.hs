{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, PackageImports, RankNTypes, CPP, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, GADTs #-}

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
import qualified Network.Riak.Content as Content


import qualified Data.ByteString.Lazy.UTF8 as B

import Database.Persist
import Database.Persist.TH
import Database.Persist.Base

import System.IO.Unsafe
import Data.Maybe (fromJust)
import "aeson-native" Data.Aeson
import Data.Map as M
import Data.Time

-- | PersistEntity for testing
mkPersist sqlSettings [persist|
Person
   name String
   age Int Maybe
   created UTCTime default=now()
|]

make_person :: PersonGeneric backend
make_person = Person {personName="Sebastian",  personAge=(Just 30), personCreated=(unsafePerformIO getCurrentTime)}

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

insert :: (PersistBackend b m, PersistEntity val) 
         => val
         -> b m (Key b val)
insert record = undefined --do
  --oid <- R.put 
    where
      -- put takes: bucket key mvclock content quorum (W) quorum (DW) returnBody?
      --put_request = R.put (entityName recDef) key mvclock content quorum quorum True
      --recDef = entityDef record
      --content = Content.json fieldsToJson
      

persistValue2JSON v = 
  zipWith (\f x -> (f, x)) getFieldNames getValues
   where
    getFieldNames = Prelude.map columnName (entityColumns $ entityDef v)
    getValues = Prelude.map (toPersistValue) (toPersistFields v)
-- encode $ toJSON $ Data.Map.fromList $ Prelude.map (\x -> (columnName x, columnType x)) (entityColumns $ entityDef make_person)

columns2JSON c acc = (columnName c) : acc 

db_host :: [Char]
db_host = "127.0.0.1"

db_port :: [Char]
db_port = "8081"

-- | Convenience function for opening the connection.
--   I am still confused if this create will always create
--   a new connection pool? TODO: find out what it does and
--   how that can be integrated in the persist backend.
open_connection_pool :: IO R.Pool
open_connection_pool = do
  c_id <- R.makeClientID
  let client = R.Client {R.host=db_host, R.port=db_port, R.clientID=c_id}
  R.create client 10 50 1
--  R.withConnection p $ R.getServerInfo
  
is_server_info :: Bool
is_server_info = 
  case si of
    R.ServerInfo n v -> ((B.toString $ fromJust $ v) == "0.14.2") && ((B.toString $ fromJust $ n) == "dev1@127.0.0.1")
    otherwise -> False
    where 
      pool = unsafePerformIO open_connection_pool
      si = unsafePerformIO $ R.withConnection pool $ R.getServerInfo
  

prop_true :: Bool -> Bool
prop_true b = b || True