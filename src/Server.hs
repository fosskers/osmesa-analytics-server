{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import qualified Data.Text as T
import           Data.Text.Lazy (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Word (Word32)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.Docs
import           Servant.Server
import           Test.QuickCheck (Arbitrary, generate, arbitrary)
import           Types

---

type OSMesa = "users" :> Get '[JSON] [LightUser]
  :<|> "users"     :> "hashtag" :> Capture "tag" T.Text :> Get '[JSON] [LightUser]
  :<|> "users"     :> Capture "uid" Word32  :> Get '[JSON] (Maybe User)
  :<|> "users"     :> Capture "name" T.Text :> Get '[JSON] (Maybe Word32)
  :<|> "hashtags"  :> Get '[JSON] [Campaign]
  :<|> "hashtags"  :> Capture "tag" T.Text  :> Get '[JSON] (Maybe Campaign)
  :<|> "help"      :> Raw

instance ToCapture (Capture "tag" T.Text) where
  toCapture _ = DocCapture "tag" "(String) Campaign hashtag to filter by."

instance ToCapture (Capture "uid" Word32) where
  toCapture _ = DocCapture "uid" "(Int) User `uid` to search by."

instance ToCapture (Capture "name" T.Text) where
  toCapture _ = DocCapture "name" "(String) Username to search by."

api :: Proxy OSMesa
api = Proxy

usage :: ByteString
usage = encodeUtf8 . pack . markdown $ docs api

server :: Server OSMesa
server = whatever :<|> const whatever :<|> user :<|> const whatever
  :<|> (simplify tag <$> whatever) :<|> hashtag
  :<|> Tagged serveDocs
  where serveDocs _ respond = respond $ responseLBS ok200 [("Content-Type", "text/plain")] usage

app :: Application
app = serve api server

whatever :: Arbitrary a => Handler a
whatever = liftIO $ generate arbitrary

user :: Word32 -> Handler (Maybe User)
user i = (\u -> Just $ u { uid = i }) <$> whatever

hashtag :: T.Text -> Handler (Maybe Campaign)
hashtag t = (\c -> Just $ c { tag = t }) <$> whatever

main :: IO ()
main = W.run 8081 app
