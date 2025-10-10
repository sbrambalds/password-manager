module Functions.Clipboard
  ( copyToClipboard
  , getClipboardContent
  )
  where

import Control.Alt ((<#>))
import Control.Apply ((<$>))
import Control.Bind (join, (=<<))
import Data.Either (hush)
import Data.Function ((#), (<<<), (>>>))
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Data.Unit (Unit)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Promise.Aff (toAffE)
import Web.Clipboard (Clipboard, clipboard, readText, writeText)
import Web.HTML (window)
import Web.HTML.Window (navigator)

maybeClipboard :: Aff (Maybe Clipboard)
maybeClipboard = (clipboard =<< navigator =<< window) # liftEffect

copyToClipboard :: String -> Aff (Maybe Unit)
copyToClipboard string = attempt ((\clip -> sequence ((toAffE <<< writeText string) <$> clip)) =<< maybeClipboard) <#> (hush >>> join)

getClipboardContent :: Aff (Maybe String)
getClipboardContent = attempt ((\clip -> sequence ((toAffE <<< readText) <$> clip)) =<< maybeClipboard) <#> (hush >>> join)