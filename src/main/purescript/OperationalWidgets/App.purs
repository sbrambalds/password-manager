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
import Functions.Handler.GenericHandlerFunctions (OperationState)
import Functions.Handler.LoginPageEventHandler (handleLoginPageEvent)
import Functions.Handler.SignupPageEventHandler (getLoginFormData, handleSignupPageEvent)
import Functions.Handler.UserAreaEventHandler (handleUserAreaEvent)
import Functions.State (getProxyInfoFromProxy, updateProxy)
import Functions.Timer (resetTimer)
import Views.AppView (PageEvent(..), appView)
import Views.LoginFormView (LoginPageEvent(..))
import Views.OverlayView (hiddenOverlayInfo)
import Views.SignupFormView (emptyDataForm)

app :: forall a. AppState -> Fragment.FragmentState -> Widget HTML a
app appState@{proxy} fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation        appState                                                                               (LoginPageEvent $ LoginEvent cred)
    Fragment.Registration -> appLoop                 (Tuple appState (WidgetState hiddenOverlayInfo (Signup  emptyDataForm)             proxyInfo))
    _                     -> appLoop                 (Tuple appState (WidgetState hiddenOverlayInfo (Login $ getLoginFormData appState) proxyInfo))

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
