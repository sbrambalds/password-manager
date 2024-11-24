module Test.Debug where

import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Concur.React.DOM (button, span, text)
import Concur.React.Props as Props
import Control.Alt (void)
import Control.Alternative ((*>))
import Control.Bind (bind)
import Data.Argonaut.Core (stringify)
import Data.Codec.Argonaut as CA
import Data.Eq ((==))
import Data.Function ((#), ($))
import DataModel.WidgetState (WidgetState)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (currentCommit)
import Test.DebugCodec (widgetStateCodec)

foreign import formatJsonString :: String -> String

debugState :: forall a. WidgetState -> Widget HTML a
debugState widgetState = do
  commit <- liftEffect $ currentCommit
  _ <- if   commit == "development" 
    then do
      let jsonEncodedState = stringify $ CA.encode widgetStateCodec widgetState
      ((button [Props._id "DEBUG", Props.onClick] [span [] [text "DEBUG"]] # void)) *> (affAction $ copyToClipboard jsonEncodedState)
    else emptyEl
  debugState widgetState

emptyEl :: forall a. Widget HTML a
emptyEl = text ""
