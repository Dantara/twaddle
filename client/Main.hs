{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Common
import           Data.Aeson
import           Miso
import           Miso.String

data Screen = Home
  | Chat
  deriving (Eq, Show)

data Message = Message {
    nickname :: MisoString
  , content  :: MisoString
  } deriving (Eq, Show)

data Model = Model {
  screen     :: Screen
  , messages :: [Message]
  } deriving (Eq, Show)

data Action
  = Init
  | Connect
  | SendMessage
  deriving (Eq, Show)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Init          -- initial action to be executed on application load
    model  = Model Home []        -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Just "main-container" -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

updateModel :: Action -> Model -> Effect Action Model
updateModel Init m               = noEff m
updateModel Connect (Model _ ms) = noEff (Model Chat ms)

viewModel :: Model -> View Action
viewModel m@(Model Home _) = home m
viewModel m@(Model Chat _) = chat m

home :: Model -> View Action
home x = div_ [] [
    div_ [class_ "jumbotron jumbotron-fluid"] [
        div_ [class_ "container"] [
              h1_ [class_ "display-4"] [text "Twaddle"]
            , p_ [class_ "lead"] [text "Secure chat in your browser"]
                                  ]
                                                        ]
  , form_ [] [
        div_ [class_ "form-group row"] [
            label_ [class_ "col-md-3 col-form-label"] [text "Your nickname: "]
            , div_ [class_ "col-md-9"] [
                input_ [class_ "form-control", placeholder_ "Nickname"]
                                        ]
                                       ]
        , div_ [class_ "form-group row"] [
            label_ [class_ "col-md-3 col-form-label"] [text "Your token: "]
            , div_ [class_ "col-md-9"] [
                input_ [readonly_ True, class_ "form-control-plaintext", value_ "kjhjhgug76kh213"]
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
  ]

chat :: Model -> View Action
chat x = div_ [] [
  text "Chat"
                 ]
