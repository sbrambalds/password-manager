module OperationalWidgets.ShareWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, p, text)
import Concur.React.Props as Props
import Control.Bind (bind, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.OneTimeShare (share)
import Functions.EnvironmentalVariables (currentCommit, redeemURL)
import Views.ShareView (Secret, shareView)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

shareWidget :: Secret -> Widget HTML Unit
shareWidget secret = do
  version <- liftEffect currentCommit
  do
    secretInfo <- shareView secret
    result <- liftAff $ runExceptT $ share secretInfo
    case result of
      Left err -> text ("error:" <> show err)
      Right id -> do
        redeemURL_ <- liftEffect $ redeemURL
        origin_  <- liftEffect $ window >>= location >>= origin
        go (redeemURL_ <> id) origin_
    <> p [Props.className "version"] [text version]
  
  where
    go :: String -> String -> Widget HTML Unit
    go url origin_ = do    
      _ <- div [Props.className "redeemSecret"] [
        a [Props.href url, Props.target "_blank"] [text "Share Link"]
      , button [(copyToClipboard (origin_ <> url)) <$ Props.onClick] [text "copy"]
      ]
      go url origin_
