{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, PackageImports, RankNTypes, CPP, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Database.Persist.Riak
     ( withRiakConn
     , runRiakConn
     , R.Connection
     , ConnectionPool
     , HostName
     , toBS
     , fromBS
     , module Database.Persist
     , RiakReader
     ) where

import Database.Persist
import Database.Persist.Base
import Database.Persist.TH

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.CatchIO
import Control.Applicative
import Control.Monad.Trans.Reader
import Network.Socket (HostName)
import qualified Data.ByteString.Lazy.UTF8 as B
import "aeson-native" Data.Aeson
-- import "aeson-native" Data.Aeson.TH
import Data.Time (Day, TimeOfDay, UTCTime)

import qualified Data.Map as M
import qualified Data.Text as T

-- Riak database library
import qualified Network.Riak as R
import qualified Network.Riak.Connection as R
import qualified Network.Riak.Connection.Pool as R
import qualified Network.Riak.Request as Req

data Tnh = Tnb Int
     | Tnc String
-- | Instance declarations for converting PersistValue to JSON
-- $(deriveJSON (drop 1) Tnh)
-- does not compile for me ;-)


-- | Connection pool for riak connections taken from riak library.
type ConnectionPool = R.Pool

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
     -> (R.Connection -> IO b)
     -> IO b
withRiakConn host port poolSize cr = do 
  cID <- R.makeClientID
  let cl = R.Client {R.host=host, R.port=port, R.clientID=cID}
  pool <- liftIO $ R.create cl poolSize 50 1
  R.withConnection pool cr 
 
newtype RiakReader m a = RiakReader (ReaderT R.Connection m a)
  deriving (Monad, MonadIO, MonadTrans, MonadCatchIO, Functor, Applicative)
  
runRiakConn :: RiakReader IO a -> R.Pool -> IO a
runRiakConn (RiakReader r) pool = R.withConnection pool $ runReaderT r
  
-- Data.ByteString.Lazy.Char8 as L
-- pack!

-- cID <- makeClientID
-- cl <- Client {host="", port="", clientID=}
-- c <- connect cl

instance MonadIO m =>
         PersistBackend RiakReader m where
   insert val = undefined
   replace i' val = undefined
   get eid' = undefined
--       let def = entityName $ entityDef $ dummyFromKey eid'
--       let eid = show $ eid'
--       let q = Req.get (B.fromString def) (B.fromString eid) R.All
--       d <- R.exchange q
--       return d

   getBy uniq = undefined
   selectFirst filts ords = undefined
   selectKeys filts = undefined
   selectEnum filts opts = undefined
   update k upds = undefined
   updateWhere filts upds = undefined
   delete k = undefined
   deleteBy uniq = undefined
   deleteWhere filts = undefined
   count filts = undefined
     
dummyFromKey _ = error "dummyFromKey"


persisttext, persistbytestring :: T.Text
persisttext = "PersistText"
persistbytestring = "PersistByteString"

instance ToJSON PersistValue where
         toJSON (PersistText t) = object [ "PersistText" .= toJSON t] 
         toJSON (PersistByteString bs) = object ["PersistByteString" .= toJSON bs]
         toJSON (PersistInt64 n) = object ["PersistInt64" .= toJSON n]
         toJSON (PersistDouble n) = object ["PersistDouble" .= toJSON n]
         toJSON (PersistBool b) = object ["PersistBool" .= toJSON b]
         toJSON (PersistDay d) = object ["PersistDay" .= (toJSON $ show d)]
         toJSON (PersistTimeOfDay t) = object ["PersistTimeOfDay" .= (toJSON $ show t)]
         toJSON (PersistUTCTime t) = object ["PersistUTCTime" .= toJSON t]
         toJSON (PersistNull) = object ["PersistNull" .= toJSON ([]::[()])]
         toJSON (PersistList l) = object ["PersistList" .= toJSON l]
         toJSON (PersistMap m) = object ["PersistMap" .= toJSON m]
         toJSON (PersistObjectId n) = object ["PersistObjectId" .= toJSON n]

instance FromJSON PersistValue where                               
         parseJSON (Object (M.toList -> [(key, value)]))
             | key == "PersistText" = PersistText <$> parseJSON value
             | key == "PersistByteString" = PersistByteString <$> parseJSON value
             | key == "PersistInt64" = PersistInt64 <$> parseJSON value
             | key == "PersistDouble" = PersistDouble <$> parseJSON value
             | key == "PersistBool" = PersistBool <$> parseJSON value
             | key == "PersistDay" = read <$> parseJSON value
             | key == "PersistTimeOfDay" = read <$> parseJSON value
             | key == "PersistUTCTime" = read <$> parseJSON value
             | key == "PersistNull" = pure PersistNull
             | key == "PersistList" = PersistList <$> parseJSON value
             | key == "PersistMap" = PersistMap <$> parseJSON value
             | key == "PersistObjectId" = PersistObjectId <$> parseJSON value

-- data PersistValue = PersistText T.Text
--                   | PersistByteString ByteString
--                   | PersistInt64 Int64
--                   | PersistDouble Double
--                   | PersistBool Bool
--                   | PersistDay Day
--                   | PersistTimeOfDay TimeOfDay
--                   | PersistUTCTime UTCTime
--                   | PersistNull
--                   | PersistList [PersistValue]
--                   | PersistMap [(T.Text, PersistValue)]
--                   | PersistObjectId ByteString -- ^ intended especially for MongoDB backend
--     deriving (Show, Read, Eq, Typeable, Ord)