module OperationalWidgets.App ( app ) where

import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Control.Alt ((<|>))
import Control.Alternative (empty, pure, (*>), (<*))
import Control.Bind (bind, (=<<), (>>=))
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.FragmentState as Fragment
import DataModel.Proxy (ProxyInfo(..))
import DataModel.WidgetState (Page(..), WidgetState(..))
import Effect.Class (liftEffect)
import Functions.Events (online)
import Functions.Handler.CardManagerEventHandler (handleCardManagerEvent)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultPagesState, pagesInfoWithLogin)
import Functions.Handler.LoginPageEventHandler (handleLoginPageEvent)
import Functions.Handler.SignupPageEventHandler (getLoginFormData, handleSignupPageEvent)
import Functions.Handler.UserAreaEventHandler (handleUserAreaEvent)
import Functions.State (getProxyInfoFromProxy, updateProxy)
import Functions.Timer (resetTimer)
import Views.AppView (PageEvent(..), appView)
import Views.LoginFormView (LoginPageEvent(..))
import Views.OverlayView (hiddenOverlayInfo)

app :: forall a. AppState -> Fragment.FragmentState -> Widget HTML a
app appState@{proxy, passkeyExists} fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation        appState (LoginPageEvent $ LoginEvent cred)
    Fragment.Registration -> appLoop                 (Tuple appState (WidgetState hiddenOverlayInfo (Tuple Signup defaultPagesState)                 proxyInfo))
    _                     -> 
      case passkeyExists of 
        false             -> appLoop                 (Tuple appState (WidgetState hiddenOverlayInfo (pagesInfoWithLogin (getLoginFormData appState)) proxyInfo))
        true              -> appWithInitialOperation        appState (LoginPageEvent LoginPasskeyEvent)

  where
    proxyInfo :: ProxyInfo
    proxyInfo = getProxyInfoFromProxy proxy

    appWithInitialOperation :: AppState -> PageEvent -> Widget HTML a
    appWithInitialOperation state event = do
      appLoop =<< executeOperation event state proxyInfo fragmentState

    appLoop :: OperationState -> Widget HTML a
    appLoop (Tuple state widgetState@(WidgetState overlayInfo page proxyInfo')) = do
      ( ( do
          resultEvent <- appView widgetState
          (executeOperation resultEvent state proxyInfo' fragmentState) <* (resetTimer # liftEffect)
        )
        <|>
        ( if (proxyInfo' == Static)
          then empty
          else affAction online *> do
                newProxy <- liftEffect $ updateProxy state
                pure $ (Tuple state {proxy = newProxy} (WidgetState overlayInfo page (getProxyInfoFromProxy newProxy)))
        )  
      )
      >>= appLoop

executeOperation :: PageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState
executeOperation (SignupPageEvent          event)      = handleSignupPageEvent   event
executeOperation (LoginPageEvent           event)      = handleLoginPageEvent    event
executeOperation (MainPageCardManagerEvent event s)    = handleCardManagerEvent  event s
executeOperation (MainPageUserAreaEvent    event s s') = handleUserAreaEvent     event s s'
executeOperation (DonationPageEvent        event)      = handleDonationPageEvent event
