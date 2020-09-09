{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Common where

import           Data.Aeson
import           Data.ByteString
import           Data.Text
import           GHC.Generics

newtype Token = Token {
  getToken :: Text
  } deriving (Show, Generic)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token
        <$> v .: "token"

instance ToJSON Token where
    toJSON (Token token) =
        object ["token" .= token]

    toEncoding (Token token) =
        pairs ("token" .= token)
