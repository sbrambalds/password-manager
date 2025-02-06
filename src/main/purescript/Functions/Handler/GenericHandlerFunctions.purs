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
import Data.List (List)
import Data.Maybe (fromMaybe)
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (emptyCredentials)
import DataModel.WidgetState (LoginType(..), Page(..), WidgetState(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.State (getProxyInfoFromProxy)
import OperationalWidgets.Sync (SyncData, SyncOperation, addPendingOperation)
import Unsafe.Coerce (unsafeCoerce)
import Views.AppView (appView)
import Views.DeviceSyncView (EnableSync)
import Views.LoginFormView (emptyLoginFormData)
import Views.OverlayView (OverlayColor, OverlayStatus(..))

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

defaultErrorPage :: Page
defaultErrorPage = Login emptyLoginFormData

handleOperationResult :: AppState -> Page -> Boolean -> OverlayColor -> Either AppError OperationState -> Widget HTML OperationState
handleOperationResult state@{proxy} page showDone color =
  either
    manageError
    (\res@(Tuple _ (WidgetState _ page' proxyInfo')) ->
      if   showDone
      then delayOperation 500 (WidgetState { status: Done, color, message: "" } page' proxyInfo') *> pure res
      else pure res
    )

                                                
  where
    manageError :: AppError -> Widget HTML OperationState
    manageError error = 
      (liftEffect $ log $ show error) *>
      case error of
        -- _ -> ErrorPage --TODO
        ProtocolError MaxPinAttemptsReachedError -> do
          delayOperation 500                       (WidgetState { status: Failed, color, message: "Max pin attempts" } page (getProxyInfoFromProxy proxy))
          pure $ Tuple (state {pinExists = false}) (WidgetState { status: Hidden, color, message: ""                 } (Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = CredentialLogin}) (getProxyInfoFromProxy proxy))
        _ -> do
          delayOperation 500 (WidgetState { status: Failed, color, message: "error" } page (getProxyInfoFromProxy proxy))
          pure $ Tuple state (WidgetState { status: Hidden, color, message: ""      } page (getProxyInfoFromProxy proxy))

delayOperation :: Int -> WidgetState -> Widget HTML Unit
delayOperation time widgetState = ((affAction $ delay (Milliseconds $ toNumber time)) <|> (unit <$ appView widgetState))

noOperation :: OperationState -> Widget HTML OperationState 
noOperation operationState@(Tuple _ widgetState) = (pure operationState) <|> (unsafeCoerce unit <$ appView widgetState)
