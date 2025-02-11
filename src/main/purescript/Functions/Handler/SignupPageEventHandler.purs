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
import DataModel.WidgetState (LoginFormData, LoginType(..), WidgetState(..))
import Functions.Communication.Signup (signupUser)
import Functions.Handler.GenericHandlerFunctions (OperationState, handleOperationResult, noOperation, pagesInfoWithLogin, pagesInfoWithSignup, runStep)
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
    ProxyResponse newProxy signupResult <- (signupUser {proxy, hashFunc: hash, srpConf, c: hex "", p: hex ""} cred) # (WidgetState (spinnerOverlay "registering" Black) pagesInfo proxyInfo # runStep)
    res                                 <-  loginSteps cred (state {proxy = newProxy}) fragmentState pagesInfo proxyInfo signupResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state pagesInfo true Black

  where
    pagesInfo = pagesInfoWithSignup (getSignupDataFromCredentials cred)

handleSignupPageEvent (GoToLoginEvent cred) state proxyInfo _ = noOperation $ Tuple state (WidgetState hiddenOverlayInfo (pagesInfoWithLogin (getLoginFormData state) {credentials = cred}) proxyInfo)
