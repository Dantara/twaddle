{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

type API = StaticAPI
              -- :<|> "chat" :> Raw
type StaticAPI = "static" :> Raw

server :: Server API
server = staticServer

staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "../static"

proxyAPI :: Proxy API
proxyAPI = Proxy

app :: Application
app = serve proxyAPI server

port :: Int
port = 3000

main :: IO ()
main = do
  putStrLn $ "Starting the server at "
    <> show(port) <> " port. "
  run port app
