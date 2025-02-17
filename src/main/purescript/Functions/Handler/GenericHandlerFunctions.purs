module Functions.Handler.GenericHandlerFunctions where

import Concur.Core (Widget, liftWidget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML, affAction)
import Control.Alt ((<|>))
import Control.Alternative ((*>), (<*))
import Control.Applicative (pure)
import Control.Bind (discard, (=<<))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either, either)
import Data.Function ((#), ($))
import Data.Functor ((<$))
import Data.Int (toNumber)
import Data.Lens (set)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (emptyCredentials)
import DataModel.WidgetState (LoginFormData, LoginType(..), MainPageWidgetState, Page(..), PagesState, WidgetState(..), _loginPagesState, _mainPagesState, _signupPagesState)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Donations (DonationLevel(..))
import Functions.State (getProxyInfoFromProxy)
import OperationalWidgets.Sync (SyncData, SyncOperation, addPendingOperation)
import Unsafe.Coerce (unsafeCoerce)
import Views.AppView (appView, emptyMainPageWidgetState)
import Views.DeviceSyncView (EnableSync)
import Views.LoginFormView (emptyLoginFormData)
import Views.OverlayView (OverlayColor, OverlayStatus(..))
import Views.SignupFormView (SignupDataForm, emptyDataForm)

type OperationState = Tuple AppState WidgetState

foreign import _operationDelay :: Unit -> Effect Number

operationDelay :: Effect Number 
operationDelay = _operationDelay unit

runStep :: forall a. WidgetState -> ExceptT AppError Aff a -> ExceptT AppError (Widget HTML) a
runStep widgetState step = ExceptT $ ((step # runExceptT # affAction) <* ((affAction <<< delay <<< Milliseconds) =<< (liftEffect operationDelay))) <|> (defaultView widgetState)

runWidgetStep :: forall a. WidgetState -> Widget HTML a -> ExceptT AppError (Widget HTML) a
runWidgetStep widgetState step = liftWidget $ ( step                           <* ((affAction <<< delay <<< Milliseconds) =<< (liftEffect operationDelay))) <|> (defaultView widgetState)

syncLocalStorage :: EnableSync -> Wire (Widget HTML) SyncData -> List SyncOperation -> WidgetState -> ExceptT AppError (Widget HTML) Unit
syncLocalStorage enableSync syncDataWire syncOperations message = 
  if enableSync
  then runWidgetStep message (addPendingOperation syncDataWire syncOperations)
  else pure unit

defaultView :: forall a. WidgetState -> Widget HTML a
defaultView widgetState = (unsafeCoerce unit <$ appView widgetState)

defaultPagesState :: PagesState
defaultPagesState = { loading: Nothing
                    , login: emptyLoginFormData
                    , signup: emptyDataForm
                    , main: emptyMainPageWidgetState
                    , donation: DonationOk
                    }

pagesInfoWithMain :: MainPageWidgetState -> Tuple Page PagesState
pagesInfoWithMain mainPageState = Tuple Main (set _mainPagesState mainPageState defaultPagesState)

pagesInfoWithLogin :: LoginFormData -> Tuple Page PagesState
pagesInfoWithLogin loginFormData = Tuple Login (set _loginPagesState loginFormData defaultPagesState)

pagesInfoWithSignup :: SignupDataForm -> Tuple Page PagesState
pagesInfoWithSignup signupDataForm = Tuple Signup (set _signupPagesState signupDataForm defaultPagesState)

defaultErrorPage :: Tuple Page PagesState
defaultErrorPage = Tuple Login defaultPagesState

handleOperationResult :: AppState -> Tuple Page PagesState -> Boolean -> OverlayColor -> Either AppError OperationState -> Widget HTML OperationState
handleOperationResult state@{proxy} (Tuple page pagesState) showDone color =
  either
    manageError
    (\res@(Tuple _ (WidgetState _ pagesInfo' proxyInfo')) ->
      if   showDone
      then delayOperation 500 (WidgetState { status: Done, color, message: "" } pagesInfo' proxyInfo') *> pure res
      else pure res
    )

                                                
  where
    manageError :: AppError -> Widget HTML OperationState
    manageError error = 
      (liftEffect $ log $ show error) *>
      case error of
        -- _ -> ErrorPage --TODO
        OperationCanceled -> do
          pure $ Tuple state (WidgetState { status: Hidden, color, message: "" } (Tuple page pagesState) (getProxyInfoFromProxy proxy))
        OperationUnsupported -> do
          delayOperation 500 (WidgetState { status: Failed, color, message: "Unsupported" } (Tuple page pagesState) (getProxyInfoFromProxy proxy))
          pure $ Tuple state (WidgetState { status: Hidden, color, message: ""            } (Tuple page pagesState) (getProxyInfoFromProxy proxy))
        ProtocolError MaxPinAttemptsReachedError -> do
          delayOperation 500                       (WidgetState { status: Failed, color, message: "Max pin attempts" } (Tuple page pagesState) (getProxyInfoFromProxy proxy))
          pure $ Tuple (state {pinExists = false}) (WidgetState { status: Hidden, color, message: ""                 } (Tuple Login defaultPagesState {login = emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = CredentialLogin}}) (getProxyInfoFromProxy proxy))
        _ -> do
          delayOperation 500 (WidgetState { status: Failed, color, message: "error" } (Tuple page pagesState) (getProxyInfoFromProxy proxy))
          pure $ Tuple state (WidgetState { status: Hidden, color, message: ""      } (Tuple page pagesState) (getProxyInfoFromProxy proxy))

delayOperation :: Int -> WidgetState -> Widget HTML Unit
delayOperation time widgetState = ((affAction $ delay (Milliseconds $ toNumber time)) <|> (unit <$ appView widgetState))

noOperation :: OperationState -> Widget HTML OperationState 
noOperation operationState@(Tuple _ widgetState) = (pure operationState) <|> (unsafeCoerce unit <$ appView widgetState)
