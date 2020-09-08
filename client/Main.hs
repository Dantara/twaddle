{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Common
import           Miso
import           Miso.String

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
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

updateModel :: Action -> Model -> Effect Action Model
updateModel Init m    = noEff m
updateModel Connect m = noEff m

viewModel :: Model -> View Action
viewModel m@(Model Home _) = home m
viewModel m@(Model Chat _) = chat m
