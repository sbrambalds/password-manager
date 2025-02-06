module Functions.Communication.SyncBackend where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<#>))
import Control.Category ((<<<))
import Control.Monad.Except (ExceptT, throwError)
import Data.Function ((#), ($))
import Data.HexString (toArrayBuffer)
import Data.List (List, foldM)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, uncurry)
import DataModel.AppError (AppError(..))
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.Proxy (Proxy, getProxy)
import DataModel.UserVersions.User (RequestUserCard(..), UserCard(..))
import DataModel.WidgetState (WidgetState)
import Functions.Communication.Blobs (deleteBlob, postBlob)
import Functions.Communication.Users (changeUserPassword, deleteUserCard, updateUserCard)
import Functions.Handler.GenericHandlerFunctions (runStep)
import OperationalWidgets.Sync (SyncOperation(..))

syncBackend :: ConnectionState -> List (Tuple SyncOperation String) -> (String -> WidgetState) -> ExceptT AppError (Widget HTML) Proxy
syncBackend connectionState syncOperations showMessage = foldM (uncurry <<< syncOperation) connectionState.proxy syncOperations

  where
    syncOperation :: Proxy -> SyncOperation -> String -> ExceptT AppError (Widget HTML) Proxy
    syncOperation newProxy op message = do
      let newConnectionState = connectionState {proxy = newProxy}
      (
        case op of
          SaveBlobFromRef _         -> throwError $ InvalidOperationError "Cannot save blob to backend just with reference"
          SaveBlob   ref id blob    -> postBlob       newConnectionState (toArrayBuffer blob) ref id <#> getProxy
          DeleteBlob ref id         -> deleteBlob     newConnectionState                      ref id <#> getProxy
          SaveUser   user           -> case user of
                                        RequestUserCard {c, masterKey, originMasterKey: Just originMasterKey} ->
                                          updateUserCard newConnectionState c (UserCard { masterKey, originMasterKey }) <#> getProxy
                                        _ ->
                                          throwError $ InvalidOperationError "Cannot save user without originMasterKey"
          ChangeUserPassword c user -> changeUserPassword newConnectionState c user <#> getProxy
          DeleteUser c              -> deleteUserCard     newConnectionState c      <#> getProxy
      ) # (showMessage message # runStep) 
