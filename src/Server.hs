{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Bool (bool)
import           Data.Proxy
import qualified Data.Text as T
import           Data.Word (Word32)
import           GHC.Generics
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Servant.API
import           Servant.Server
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Environment (lookupEnv)
import           Test.QuickCheck (Arbitrary, generate, arbitrary)
import           Text.Read (readMaybe)
import           Types

---

data Env = Env { heroku :: Bool <?> "Bind to $PORT, as Heroku expects." } deriving (Generic, ParseRecord)

type OSMesa = "users" :> Get '[JSON] [LightUser]
  :<|> "users"     :> "hashtag" :> Capture "tag" T.Text :> Get '[JSON] [LightUser]
  :<|> "users"     :> Capture "uid" Word32  :> Get '[JSON] (Maybe User)
  :<|> "users"     :> Capture "name" T.Text :> Get '[JSON] (Maybe Word32)
  :<|> "hashtags"  :> Get '[JSON] [Campaign]
  :<|> "hashtags"  :> Capture "tag" T.Text  :> Get '[JSON] (Maybe Campaign)

type API = OSMesa :<|> SwaggerSchemaUI "help" "swagger.json"

server :: Server API
server = (whatever :<|> const whatever :<|> user :<|> const whatever
  :<|> (simplify tag <$> whatever) :<|> hashtag)
  :<|> swaggerSchemaUIServer (toSwagger (Proxy :: Proxy OSMesa))

app :: Application
app = serve (Proxy :: Proxy API) server

whatever :: Arbitrary a => Handler a
whatever = liftIO $ generate arbitrary

user :: Word32 -> Handler (Maybe User)
user i = (\u -> Just $ u { uid = i }) <$> whatever

hashtag :: T.Text -> Handler (Maybe Campaign)
hashtag t = (\c -> Just $ c { tag = t }) <$> whatever

main :: IO ()
main = do
  Env (Helpful h) <- getRecord "OSMesa Analytics Server"
  port <- bool (pure 8081) f h
  W.run port app
  where f = maybe 8081 id . (>>= readMaybe) <$> lookupEnv "PORT"
