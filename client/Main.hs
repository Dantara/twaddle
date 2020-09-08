{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Miso
import           Miso.String

data Screen = Credentials
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
    model  = Model Credentials [] -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

updateModel :: Action -> Model -> Effect Action Model
updateModel Init m    = noEff m
updateModel Connect m = noEff m
-- updateModel SayHelloWorld m = m <# do
--   putStrLn "Hello World" >> pure NoOp


-- viewModel :: Model -> View Action
-- viewModel x = div_ [] [
--    button_ [ onClick AddOne ] [ text "+" ]
--  , text (ms x)
--  , button_ [ onClick SubtractOne ] [ text "-" ]
--  ]

viewModel :: Model -> View Action
viewModel x = div_ [] [
    label_ [] [
        text "Your nickname:"
      , input_ []
            ]
  , label_ [] [
        text "Your token:"
      , text "kjhkguh123juy876gl"
      ]
  , label_ [] [
        text "Enter the token of other person:"
      , input_ []
            ]
  , button_ [ onClick Connect ] [text "Connect"]
                      ]
