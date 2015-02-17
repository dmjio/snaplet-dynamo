{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : Snap.Snaplet.DynamoDB
-- Copyright   : Vertigo Media Inc. (c) 2014-2015
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Snap.Snaplet.DynamoDB
    ( -- * DynamoDB
      DynamoDB 
    , runDynamo
    , dynamoConnection
    , dynamoDBInit
    , dynamoDBInitConf
    ) where

import Aws.General
import Control.Lens                ( makeLenses
                                   , Simple
                                   , Lens
                                   , view
                                   )
import Control.Retry               ( RetryPolicy )
import qualified Data.Configurator as C
import Data.Maybe                  ( fromMaybe )
import Network.HTTP.Client         ( newManager )
import Network.HTTP.Client.OpenSSL ( opensslManagerSettings )
import OpenSSL.Session             ( context )
import Snap                        ( MonadState
                                   , MonadIO
                                   , gets
                                   , liftIO
                                   )
import Snap.Snaplet                ( makeSnaplet
                                   , getSnapletUserConfig
                                   , SnapletInit
                                   , snapletValue
                                   , Snaplet
                                   )
import Text.Read                   ( readMaybe )

import Web.AWS.DynamoDB.Client     ( dynamo
                                   , defaultDynamoBackoffPolicy
                                   )
import Web.AWS.DynamoDB.Types      ( DynamoConfig (..)
                                   , DynamoAction
                                   , PublicKey    (..) 
                                   , SecretKey    (..)
                                   , DynamoError
                                   )

------------------------------------------------------------------------------
-- | Type for DynamoDB
data DynamoDB = DynamoDB {
        _config :: DynamoConfig
      } deriving (Show)

------------------------------------------------------------------------------
-- | DynamoDB Lenses
makeLenses ''DynamoDB

-- | Perform action using Dynamo connection from DynamoDB snaplet pool
-- > 
-- > runDynamo action ListTables
-- >   
runDynamo
  :: (MonadState app m, MonadIO m, DynamoAction a b, Show b)
  => Simple Lens app (Snaplet DynamoDB)
  -> a
  -> m (Either DynamoError b)
runDynamo snaplet action = do
  conf <- gets $ view (snaplet . snapletValue . config)
  liftIO $ dynamo conf action

------------------------------------------------------------------------------
-- | A lens to retrieve the connection to DynamoDB from the 'DynamoDB'
-- wrapper.
dynamoConnection :: Simple Lens DynamoDB DynamoConfig
dynamoConnection = config

------------------------------------------------------------------------------
-- | DynamoDB init config
dynamoDBInitConf
    :: Maybe RetryPolicy
    -> SnapletInit b DynamoDB
dynamoDBInitConf policy = makeSnaplet "dynamo" "DynamoDB snaplet" Nothing $ do
    conf <- getSnapletUserConfig
    dynamoConfig <- liftIO $ do
        cPublic <- C.lookup conf "public"
        cSecret <- C.lookup conf "secret"
        cRegion <- C.lookup conf "region"
        cDev    <- C.lookup conf "dev"
        cDebug  <- C.lookup conf "debug"
        mgr <- newManager (opensslManagerSettings context)
        return DynamoConfig { 
            dynamoPublicKey = PublicKey $ fromMaybe "public" cPublic
          , dynamoSecretKey = SecretKey $ fromMaybe "secret" cSecret
          , dynamoManager   = mgr
          , dynamoUrl       = Nothing
          , dynamoRegion    = case cRegion of
                                Nothing -> UsEast1
                                Just y  ->
                                  case fromText y :: Either String Region of
                                    Left _ -> UsEast1
                                    Right r -> r
                                    
          , dynamoBackOff   = fromMaybe defaultDynamoBackoffPolicy policy
          , dynamoIsDev     = case cDev of
                                Nothing -> True
                                Just x  ->
                                    case readMaybe x :: Maybe Bool of
                                      Nothing -> True
                                      Just y -> y
          , dynamoDebug     = case cDebug of
                                Nothing -> False
                                Just x ->
                                  case readMaybe x :: Maybe Bool of
                                    Nothing -> False
                                    Just y  -> y
        }

    return $ DynamoDB dynamoConfig

------------------------------------------------------------------------------
-- | Initialization
dynamoDBInit :: DynamoConfig -- ^ Information for connnecting to a DynamoDB server.
             -> SnapletInit b DynamoDB
dynamoDBInit conf =
    makeSnaplet "snaplet-dynamo" "DynamoDB snaplet." Nothing $ do
      return $ DynamoDB conf
