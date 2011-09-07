{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, PackageImports, RankNTypes, CPP #-}
module Database.Persist.Riak
     ( withRiakConn
     , runRiakConn
     , HostName
     , module Database.Persist
     ) where

import Database.Persist
import Database.Persist.Base

import Network.Socket (HostName)
import qualified Network.Riak.Connection.Pool as Pool

-- Riak database library
import Network.Riak
import Network.Riak.Connection

type ConnectionPool = Pool.Pool

withRiakConn = undefined
   
runRiakConn = undefined