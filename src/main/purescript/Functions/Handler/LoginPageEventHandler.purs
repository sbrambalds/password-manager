module Functions.Handler.LoginPageEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (<#>), (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.Function ((#), ($))
import Data.HexString (hex, toArrayBuffer)
import Data.Int (fromString)
import Data.Lens (view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord ((<))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.FragmentState as Fragment
import DataModel.Proxy (Proxy(..), ProxyInfo, ProxyResponse(..), defaultOnlineProxy)
import DataModel.UserVersions.User (_indexReference, _index_reference, _userInfoRef_key, _userInfoRef_reference)
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), LoginType(..), Page(..), WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Login (PrepareLoginResult, loginStep1, loginStep2, prepareLogin)
import Functions.DeviceSync (computeSyncOperations, getSyncOptionFromLocalStorage)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.EncodeDecode (importCryptoKeyAesGCM)
import Functions.Events (effectDelayed, focus)
import Functions.Handler.GenericHandlerFunctions (OperationState, delayOperation, handleOperationResult, noOperation, runStep, runWidgetStep)
import Functions.Index (decryptIndex)
import Functions.Pin (decryptPassphraseWithPin, deleteCredentials, pinFailureCountKey)
import Functions.SRP (checkM2)
import Functions.State (getProxyInfoFromProxy, updateProxy)
import Functions.Timer (activateTimer)
import Functions.User (decryptUserInfo, extractUserInfoReference)
import OperationalWidgets.Sync (addPendingOperation, updateConnectionState)
import Record (merge)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.CreateCardView (emptyCardFormData)
import Views.LoginFormView (LoginPageEvent(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SignupFormView (emptyDataForm)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

handleLoginPageEvent :: LoginPageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState 

handleLoginPageEvent (LoginEvent cred) state@{srpConf} proxyInfo fragmentState =
  do
    prepareLoginResult <- (prepareLogin srpConf cred) # (message "Prepare login" # runStep)
    res                <-  loginSteps cred state fragmentState initialPage proxyInfo prepareLoginResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage true Black

  where 
    initialPage = (Login emptyLoginFormData {credentials = cred})
    message s   = WidgetState (spinnerOverlay s Black) initialPage proxyInfo


handleLoginPageEvent (LoginPinEvent pin) state@{hash, srpConf} proxyInfo fragmentState = do
  do
    cred               <- (decryptPassphraseWithPin hash pin) # (message "Decrypt with PIN" # runStep)
    prepareLoginResult <- (prepareLogin srpConf cred)         # (message "Prepare login"    # runStep)
    res                <-  loginSteps cred state fragmentState initialPage proxyInfo prepareLoginResult
    pure res
  
  # runExceptT
  >>= handlePinResult       state initialPage      Black
  >>= handleOperationResult state emptyPage   true Black

  where
    initialPage = Login emptyLoginFormData {pin = pin, loginType = PinLogin}
    emptyPage   = Login emptyLoginFormData {loginType = PinLogin}
    message s   = WidgetState (spinnerOverlay s Black) initialPage proxyInfo

handleLoginPageEvent (UpdateForm loginFormData)          state proxyInfo _ = noOperation (Tuple state (WidgetState hiddenOverlayInfo (Login loginFormData)                                                           proxyInfo))

handleLoginPageEvent (GoToSignupEvent cred)              state proxyInfo _ = noOperation (Tuple state (WidgetState hiddenOverlayInfo (Signup emptyDataForm     {username = cred.username, password = cred.password}) proxyInfo))

handleLoginPageEvent (GoToCredentialLoginEvent username) state proxyInfo _ = noOperation (Tuple state (WidgetState hiddenOverlayInfo (Login emptyLoginFormData {credentials = {username, password: ""}            }) proxyInfo))

-- ========================================================================================================================

loginSteps :: Credentials -> AppState -> Fragment.FragmentState -> Page -> ProxyInfo -> PrepareLoginResult -> ExceptT AppError (Widget HTML) OperationState
loginSteps cred state@{proxy, hash: hashFunc, srpConf} fragmentState page proxyInfo prepareLoginResult = do
  let connectionState = {proxy, hashFunc, srpConf, c : hex "", p: hex ""}

  ProxyResponse proxy'   loginStep1Result <- (loginStep1         connectionState                 prepareLoginResult.c                                      ) # (message "SRP step 1" # runStep)
  ProxyResponse proxy''  loginStep2Result <- (loginStep2         connectionState{proxy = proxy'} prepareLoginResult.c prepareLoginResult.p loginStep1Result) # (message "SRP step 2" # runStep)
  _                                       <- ((liftAff $ checkM2 srpConf loginStep1Result.aa loginStep2Result.m1 loginStep2Result.kk (toArrayBuffer loginStep2Result.m2)) >>= (\result -> 
                                                if result
                                                then pure         unit
                                                else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
                                              ))                                                                                                             # (message "Validate user" # runStep)
  userInfoReferences                      <- (  extractUserInfoReference loginStep2Result.masterKey 
                                                =<< 
                                                (importCryptoKeyAesGCM (prepareLoginResult.p # toArrayBuffer) # liftAff)
                                              )                                                                                                              # (message "Validate user" # runStep)
  let stateUpdate = { masterKey:          Just loginStep2Result.masterKey 
                    , userInfoReferences: Just userInfoReferences
                    , username:           Just cred.username
                    , password:           Just cred.password
                    , s:                  Just loginStep1Result.s
                    , c:                  Just prepareLoginResult.c
                    , p:                  Just prepareLoginResult.p
                    }

  res                                     <- loadHomePageSteps (merge stateUpdate (state {proxy = proxy''})) page proxyInfo fragmentState

  pure $ res

  where 
    message s = WidgetState {status: Spinner, color: Black, message: s} page proxyInfo

loadHomePageSteps :: AppState -> Page -> ProxyInfo -> Fragment.FragmentState -> ExceptT AppError (Widget HTML) OperationState

loadHomePageSteps state@{hash: hashFunc, proxy, srpConf, c: Just c, p: Just p, masterKey: Just (Tuple _ masterKeyEncodingVersion), userInfoReferences: Just userInfoReferences, syncDataWire} page proxyInfo fragmentState = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  ProxyResponse proxy'  userInfo <- (do 
                                        ProxyResponse newProxy blob <- getBlob connectionState (view _userInfoRef_reference userInfoReferences)
                                        decryptUserInfo blob (view _userInfoRef_key userInfoReferences) masterKeyEncodingVersion <#> ProxyResponse newProxy
                                    ) # (message "Get user info" # runStep)
  ProxyResponse proxy'' index    <- (do 
                                        ProxyResponse newProxy blob <- getBlob connectionState{ proxy = proxy'} (view _index_reference userInfo)
                                        decryptIndex (view _indexReference userInfo) blob <#> ProxyResponse newProxy
                                    ) # (message "Get index"     # runStep)
  donationLevel                  <- (computeDonationLevel index userInfo # liftEffect ) # (message "Compute donation level" # runStep)                                                     
  
  case (unwrap (unwrap userInfo).userPreferences).automaticLock of
    Right n -> liftEffect (activateTimer n)
    Left  _ -> pure unit

  let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)
                        _                     -> NoCard
  enableSync     <- (getSyncOptionFromLocalStorage c # liftEffect) # (message "Compute data to sync" # runStep)

  let updatedState = state {proxy = proxy'', index = Just index, userInfo = Just userInfo, donationLevel = Just donationLevel, enableSync = enableSync}

  syncOperations <- (computeSyncOperations updatedState                                                                  ) # (message "Compute data to sync" # runStep)
  _              <- (updateConnectionState syncDataWire {c, p, srpConf, hashFunc, proxy: DynamicProxy defaultOnlineProxy}) # (message "Compute data to sync" # runWidgetStep)
  _              <- (addPendingOperation   syncDataWire syncOperations                                                   ) # (message "Compute data to sync" # runWidgetStep)

  proxy''' <- (updateProxy updatedState # liftEffect) # (message "Compute data to sync" # runStep)
  
  (effectDelayed 510.0 (focus "mainView" # liftEffect) # liftAff) # (message "" # runStep)

  pure $ Tuple
    updatedState { proxy = proxy'''}
    (WidgetState 
      hiddenOverlayInfo
      case donationLevel of
        DonationWarning -> (Donation donationLevel)
        _               -> (Main emptyMainPageWidgetState { index = index, cardManagerState = cardManagerInitialState { cardViewState = cardViewState }, donationLevel = donationLevel, syncDataWire = Just syncDataWire, enableSync = enableSync })
      (getProxyInfoFromProxy proxy''')
    )

  where
    message s = WidgetState {status: Spinner, color: Black, message: s} page proxyInfo

loadHomePageSteps _ _ _ _  = do
  throwError (InvalidStateError $ CorruptedState "")

handlePinResult :: AppState -> Page -> OverlayColor -> Either AppError OperationState -> Widget HTML (Either AppError OperationState)
handlePinResult {proxy} page color either = do
  let proxyInfo       = getProxyInfoFromProxy proxy
 
  storage <- liftEffect $ window >>= localStorage
  
  case either of
    Right _ -> do
        liftEffect $ setItem pinFailureCountKey (show 0) storage
        delayOperation 250 $ WidgetState (spinnerOverlay "Reset PIN attempts" color) page proxyInfo
        pure either
    Left  _ -> do
        failures <- liftEffect $ getItem pinFailureCountKey storage
        let count = ((fromMaybe 0 <<< fromString <<< fromMaybe "") failures) + 1
        if count < 3 then do
          liftEffect $ setItem pinFailureCountKey (show count) storage
          effectDelayed 510.0 (focus "loginPINInput" # liftEffect) # affAction
          pure either
        else do
          liftEffect $ deleteCredentials storage
          pure $ (Left (ProtocolError MaxPinAttemptsReachedError))
