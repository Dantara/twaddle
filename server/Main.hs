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
import           Servant.HTML.Lucid

type API = (Get '[HTML] Layout)
  :<|> StaticAPI

type StaticAPI = "static" :> Raw

data Layout = Layout

instance L.ToHtml Layout where
  toHtmlRaw = toHtml
  toHtml Layout = do
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
        L.body_ $
          L.div_ [L.class_ "container"] $
             L.div_ [L.class_ "col-md-10 offset-md-1", L.id_ "main-container"] mempty
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

app :: Application
app = serve (Proxy @ API) (pure Layout :<|> static)
  where
    static = serveDirectoryWebApp "static"

port :: Int
port = 3000

main :: IO ()
main = do
  putStrLn $ "Starting the server at "
    <> show(port) <> " port."
  run port app
