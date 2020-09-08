{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Common where

import           Miso
import           Miso.String
import           Servant.API

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

-- | Router
type ClientRoutes = HomePage
  :<|> ChatPage

-- | Handlers
handlers :: (Model -> View Action)
  :<|> (Model -> View Action)
handlers = home
  :<|> chat

type HomePage = View Action
type ChatPage = View Action

home :: Model -> View Action
home x = div_ [] [
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

chat :: Model -> View Action
chat x = div_ [] [
  text "Chat"
                 ]

