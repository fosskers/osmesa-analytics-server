{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy
import qualified Data.Text as T
import           Data.Word (Word32)
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.Server
import           Servant.Swagger
import           Servant.Swagger.UI
import           Test.QuickCheck (Arbitrary, generate, arbitrary)
import           Types

---

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
main = W.run 8081 app
