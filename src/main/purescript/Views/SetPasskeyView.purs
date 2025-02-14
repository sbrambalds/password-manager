module Views.SetPasskeyView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, form, h1, p, strong, text)
import Concur.React.Props as Props
import Data.Function (($))
import Data.Semigroup ((<>))
import Views.SimpleWebComponents (simpleButton)

data PasskeyEvent = Reset | SetPasskey

setPasskeyView :: Boolean -> Widget HTML PasskeyEvent
setPasskeyView passkeyExists = div [Props._id "passkeyPage"] [ form [] [
  h1 [] [text "Device Passkey"]
, div [Props.className "description"] $ [
    p [] [text "You may create a passkey to be used instead of your passphrase. Please note that the passkey is specific to the device you are now using."]
  , p [] [text "You will first be asked to register a passkey and then to authenticate with it."]
  , p [] [strong [] [text "Warning: "], text "This login option uses an experimental feature and may not work on your device. Unfortunately there is no way of knowing it before the passkey registration; if your device doesn't support this feature you will get the error ", strong [] [text "'Unsupported'"], text "."]
  ]
, div [Props.className "content"] [
    text $ "Passkey is " <> (if passkeyExists then "" else "not ") <> "set on this device"
  , if   passkeyExists 
    then simpleButton "reset"  "Reset"           false Reset
    else simpleButton "create" "Create Passkey"  false SetPasskey
  ]
]]
