{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, PackageImports, RankNTypes, CPP #-}
module Database.Persist.Riak
     ( withRiakConn
     , runRiakConn
     , Connection
     , ConnectionPool
     , HostName
     , toBS
     , fromBS
     , module Database.Persist
     ) where

import Database.Persist
import Database.Persist.Base

import qualified Control.Monad.IO.Class as Trans
import Control.Applicative

import Network.Socket (HostName)
import qualified Network.Riak.Connection.Pool as Pool

import qualified Data.ByteString.Lazy.UTF8 as B

-- Riak database library
import Network.Riak
import Network.Riak.Connection

-- | Connection pool for riak connections taken from riak library.
type ConnectionPool = Pool.Pool

-- | Converting to ByteString
toBS :: (Show a) => a -> B.ByteString
toBS = B.fromString . show

fromBS :: (Read a) => B.ByteString -> a
fromBS = read . B.toString

--withRiakConn :: (Trans.MonadIO m, Applicative m)   
--               => String 
--               -> String 
--               -> Int 
               -- -> (Pool.Pool -> m b) 
--               -> m b

withRiakConn
  :: HostName
     -> String
     -> Int
     -> (Connection -> IO b)
     -> IO b
withRiakConn h p i cr = do 
  cID <- makeClientID
  let cl = Client {host=h, port=p, clientID=cID}
  pool <- Trans.liftIO $ Pool.create cl i 50 1
  Pool.withConnection pool cr

  

runRiakConn = undefined

-- Data.ByteString.Lazy.Char8 as L
-- pack!

-- cID <- makeClientID
-- cl <- Client {host="", port="", clientID=}
-- c <- connect cl