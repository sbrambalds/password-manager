module WidgetManagers.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Bind (bind)
import Data.Unit (Unit)
import SRP as SRP
import WidgetManagers.HomePageManager as HomePageManager
import WidgetManagers.LandingPage as LandingPage

app :: Widget HTML Unit
app = do
  loginResult <- (LandingPage.landingPage SRP.baseConfiguration)
  HomePageManager.homePageManager loginResult

  -- landingPage :: SRP.SRPConf -> StateT AppState (Widget HTML) LoginManagerResult
  -- homePageManager :: LoginManagerResult -> StateT AppState (Widget HTML) Unit
  -- Tuple loginResult newState <- runStateT (landingPage SRP.baseConfiguration) initialState
  -- void $ runStateT (homePageManager loginResult) newState