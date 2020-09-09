{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Common
import qualified Lucid                          as L
import           Lucid.Base
import           Miso
import           Miso.String
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

type API = ServerRoutes
  :<|> StaticAPI

type StaticAPI = "static" :> Raw

type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) = do
      L.doctype_
      L.html_ [ L.lang_ "en" ] $ do
        L.head_ $ do
          L.title_ "Twaddle"
          L.meta_ [ L.charset_ "utf-8" ]
          L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]
          L.meta_ [ L.name_ "viewport"
                  , L.content_ "width=device-width, initial-scale=1"
                  ]
          L.meta_ [ L.name_ "description"
                  , L.content_ "Secure chat in your browser"
                  ]
          cssRef bootstrapCssRef
          jsRef jqueryRef
          jsRef bootstrapJsRef
          cssRef "static/style.css"
          jsRef "static/all.js"
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

bootstrapCssRef :: MisoString
bootstrapCssRef = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

bootstrapJsRef :: MisoString
bootstrapJsRef = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.bundle.min.js"

jqueryRef :: MisoString
jqueryRef = "https://code.jquery.com/jquery-3.5.1.min.js"

proxyAPI :: Proxy API
proxyAPI = Proxy

app :: Application
app = serve (Proxy @ API) (serverHandlers :<|> static)
  where
    static = serveDirectoryWebApp "static"

port :: Int
port = 3000

serverHandlers ::
       Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
serverHandlers = homeHandler :<|> chatHandler
  where
    send f u = pure $ Wrapper $ f u
    homeHandler = send home (Model Home [])
    chatHandler = send chat (Model Chat [])


main :: IO ()
main = do
  putStrLn $ "Starting the server at "
    <> show(port) <> " port."
  run port app
