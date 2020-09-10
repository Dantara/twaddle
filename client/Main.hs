{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Common
import           Data.Aeson
import           Data.Proxy
import           Miso
import           Miso.String
import           Servant.Client.Ghcjs

data Screen = Home
  | Chat
  deriving (Eq, Show)

data Message = Message {
    from    :: MisoString
  , content :: MisoString
  } deriving (Eq, Show)

data Model = Model {
  screen     :: Screen
  , token    :: MisoString
  , nickname :: MisoString
  , messages :: [Message]
  } deriving (Eq, Show)

data Action
  = NoOp
  | FetchToken
  | SetToken Token
  | Connect
  | SendMessage
  | UpdateNickname MisoString
  deriving (Eq, Show)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = FetchToken    -- initial action to be executed on application load
    model  = Model Home "" "" []        -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Just "main-container" -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m                 = noEff m
updateModel Connect m = m { screen = Chat } <# do
  pure NoOp
updateModel FetchToken m = m <# do
  SetToken <$> fetchToken
updateModel (SetToken t) m =
  m {token = toMisoString $ getToken t} <# do
  pure NoOp
updateModel (UpdateNickname nick) m =
  m {nickname = nick} <# do
  pure NoOp

viewModel :: Model -> View Action
viewModel m
  | screen m == Chat = chat m
  | otherwise = home m

home :: Model -> View Action
home m = div_ [] [
    div_ [class_ "jumbotron jumbotron-fluid"] [
        div_ [class_ "container"] [
              h1_ [class_ "display-4"] [text "Twaddle"]
            , p_ [class_ "lead"] [text "Secure chat in your browser"]
                                  ]
                                                        ]
    , div_ [class_ "form-group row"] [
            label_ [class_ "col-md-3 col-form-label"] [text "Your nickname: "]
            , div_ [class_ "col-md-9"] [
                input_ [class_ "form-control", placeholder_ "Nickname"]
                                        ]
                                       ]
    , div_ [class_ "form-group row"] [
            label_ [class_ "col-md-3 col-form-label"] [text "Your token: "]
            , div_ [class_ "col-md-9"] [
                input_ [readonly_ True, class_ "form-control-plaintext", value_ (token m)]
                                        ]
                                         ]
    , div_ [class_ "form-group row"] [
            label_ [class_ "col-md-3 col-form-label"] [text "Token of your companion: "]
            , div_ [class_ "col-md-9"] [
                input_ [class_ "form-control", placeholder_ "token"]
                                        ]
                                       ]
    , button_ [onClick Connect, class_ "btn btn-primary"] [text "Connect"]
  ]

chat :: Model -> View Action
chat x = div_ [] [
  div_ [class_ "header"] [
      h1_ [] [text "Twaddle"]
                         ]
  , div_ [class_ "chat"] [
      div_ [class_ "messages-field col-sm-12"] [
          h5_ [] [text "Dantara:"]
          , p_ [] [text "Hello World!!!"]
                                                                      ]
      , div_ [class_ "form-inline sender-line"] [
          input_ [class_ "form-control col-md-10", placeholder_ "Message"]
          , button_ [onClick NoOp, class_ "btn btn-secondary col-md-2"] [text "Send"]
                               ]
                         ]
                 ]

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

tokenClient :: ClientM Token
tokenClient = client tokenApi

fetchToken :: IO Token
fetchToken = do
  resp <- runClientM tokenClient
  case resp of
    Left _ ->
      return $ Token "Error"
    Right t ->
      return t
