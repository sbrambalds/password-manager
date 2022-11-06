module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app)

main :: Effect Unit
main = do
  runWidgetInDom "app" app
