module Functions.Handler.SignupPageEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT)
import Data.Function ((#), ($))
import Data.HexString (hex)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.FragmentState as Fragment
import DataModel.Proxy (ProxyInfo, ProxyResponse(..))
import DataModel.WidgetState (LoginFormData, LoginType(..), Page(..), WidgetState(..))
import Functions.Communication.Signup (signupUser)
import Functions.Handler.GenericHandlerFunctions (OperationState, noOperation, handleOperationResult, runStep)
import Functions.Handler.LoginPageEventHandler (loginSteps)
import Views.LoginFormView (emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SignupFormView (SignupPageEvent(..), getSignupDataFromCredentials)

getLoginFormData :: AppState -> LoginFormData
getLoginFormData {pinExists: true} = emptyLoginFormData { loginType = PinLogin }
getLoginFormData _                 = emptyLoginFormData

handleSignupPageEvent :: SignupPageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState

handleSignupPageEvent (SignupEvent cred) state@{proxy, hash, srpConf} proxyInfo fragmentState = 
  do
    ProxyResponse newProxy signupResult <- runStep (signupUser {proxy, hashFunc: hash, srpConf, c: hex "", p: hex ""} cred) (WidgetState (spinnerOverlay "registering" Black) initialPage proxyInfo)
    res                                 <- loginSteps cred (state {proxy = newProxy}) fragmentState initialPage proxyInfo signupResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage true Black

  where
    initialPage = Signup $ getSignupDataFromCredentials cred

handleSignupPageEvent (GoToLoginEvent cred) state proxyInfo _ = noOperation $ Tuple state (WidgetState hiddenOverlayInfo (Login (getLoginFormData state) {credentials = cred}) proxyInfo)
