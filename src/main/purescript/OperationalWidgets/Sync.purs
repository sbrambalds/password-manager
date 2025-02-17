module OperationalWidgets.Sync where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire(..), mapWire, send, with)
import Concur.React (HTML, affAction)
import Control.Alternative ((*>))
import Control.Bind (discard, pure, (=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (runExceptT)
import Control.Plus (empty)
import Data.Argonaut.Core (stringify)
import Data.Codec (encode)
import Data.Either (Either(..))
import Data.Eq (class Eq, (/=), (==))
import Data.Function ((#), ($))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toString)
import Data.Identifier (Identifier)
import Data.Lens (Lens', addOver, set, setJust)
import Data.Lens.Record (prop)
import Data.List (List(..), deleteAt, findIndex, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import DataModel.AppError (AppError(..))
import DataModel.Communication.ConnectionState (ConnectionState, _proxy)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Proxy (ProxyResponse(..))
import DataModel.UserVersions.User (RequestUserCard, requestUserCardCodec)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Functions.Communication.Blobs (getBlob)
import Functions.Events (online)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Navigator (onLine)
import Web.HTML.Window (localStorage, navigator)
import Web.Storage.Storage (removeItem, setItem)

type SyncData = {completedOperations :: Int, pendingOperations :: List SyncOperation, connectionState :: Maybe ConnectionState}

baseSyncData :: SyncData
baseSyncData = {
  completedOperations: 0
, pendingOperations: Nil
, connectionState: Nothing
}

_completedOperations :: Lens' SyncData Int
_completedOperations = prop (Proxy :: _ "completedOperations")

_pendingOperations :: Lens' SyncData (List SyncOperation)
_pendingOperations = prop (Proxy :: _ "pendingOperations")

_connectionState :: Lens' SyncData (Maybe ConnectionState)
_connectionState = prop (Proxy :: _ "connectionState")

addPendingOperation :: Wire (Widget HTML) SyncData -> List SyncOperation -> Widget HTML Unit
addPendingOperation syncDataWire syncOperations = ((un Wire syncDataWire).value # liftEffect) >>= ((\syncData -> syncData.pendingOperations <> syncOperations) >>> send (mapWire _pendingOperations syncDataWire))

updateConnectionState :: Wire (Widget HTML) SyncData -> ConnectionState -> Widget HTML Unit
updateConnectionState syncDataWire connectionState = send (mapWire _connectionState syncDataWire) (Just connectionState)

type Reference = HexString
type Blob      = HexString 

data SyncOperation  = SaveBlobFromRef HexString 
                    | SaveBlob Reference Identifier Blob
                    | DeleteBlob HexString Identifier
                    | SaveUser RequestUserCard
                    | ChangeUserPassword HexString RequestUserCard
                    | DeleteUser HexString

instance showSyncOperation :: Show SyncOperation where
  show (SaveBlobFromRef ref)    = "SaveBlobFromRef " <> show ref
  show (SaveBlob ref _ _)       = "SaveBlob "        <> show ref
  show (DeleteBlob ref _)       = "DeleteBlob "      <> show ref
  show (SaveUser user)          = "SaveUser "        <> show (unwrap user).c
  show (ChangeUserPassword c _) = "ChangePassword "  <> show c
  show (DeleteUser ref)         = "DeleteUser "      <> show ref

derive instance eqSyncOperation :: Eq SyncOperation

executeLocalStorageSynOperations :: forall a. Wire (Widget HTML) SyncData -> Widget HTML a 
executeLocalStorageSynOperations wire = with wire \syncData -> affAction (delay $ Milliseconds 0.1) *> do
  ((onLine =<< navigator =<< window) # liftEffect)  >>= case _ of
    true  -> case syncData of
      {connectionState:   Nothing} -> empty
      {pendingOperations: Nil    } -> if syncData.completedOperations /= 0 
                                      then send (mapWire _completedOperations wire) 0
                                      else empty
      _                            -> syncOperation syncData >>= send wire
    false -> affAction online *> send wire syncData
  empty

  where
    syncOperation :: SyncData -> Widget HTML SyncData
    syncOperation syncData@{pendingOperations: (head : tail), connectionState: Just connectionState} = case head of
      SaveBlobFromRef   ref  -> do
        getBlob connectionState ref # runExceptT # affAction >>= case _ of
          Right (ProxyResponse proxy blob) -> do
            (setItem ("blob_" <> toString Hex ref) (toString Hex (fromArrayBuffer blob)) =<< localStorage =<< window) # liftEffect
            pure $ syncData #   addOver _completedOperations 1 
                            <<< set     _pendingOperations   tail 
                            <<< setJust _connectionState    (set _proxy proxy connectionState)  
          Left (ProtocolError (ResponseError 404)) ->
            case findIndex (deleteSameRef ref) tail  of
              Just i  -> pure $ syncData  #   addOver _completedOperations 2 
                                          <<< set     _pendingOperations  (fromMaybe tail $ deleteAt i tail) 
              Nothing -> empty
          _ -> pure $ syncData

      SaveBlob ref _ blob -> do
        (setItem ("blob_" <> toString Hex ref) (toString Hex blob) =<< localStorage =<< window) # liftEffect
        pure $ syncData #   addOver _completedOperations 1 
                        <<< set     _pendingOperations   tail

      DeleteBlob ref _ -> do
        (removeItem ("blob_" <> toString Hex ref) =<< localStorage =<< window) # liftEffect
        pure $ syncData #   addOver _completedOperations 1 
                        <<< set     _pendingOperations   tail

      SaveUser   user -> do
        (setItem ("user_" <> toString Hex (unwrap user).c) (stringify $ encode requestUserCardCodec user) =<< localStorage =<< window) # liftEffect
        pure $ syncData #   addOver _completedOperations 1 
                        <<< set     _pendingOperations   tail

      ChangeUserPassword oldC user -> do
        syncOperation (syncData # set _pendingOperations ((DeleteUser oldC) : (SaveUser user) : tail))

      DeleteUser c    -> do
        (removeItem ("user_" <> toString Hex c) =<< localStorage =<< window) # liftEffect
        pure $ syncData #   addOver _completedOperations 1 
                        <<< set     _pendingOperations   tail
      
    syncOperation _ = empty
    
    deleteSameRef :: HexString -> SyncOperation -> Boolean
    deleteSameRef ref = case _ of
      DeleteBlob ref' _ -> ref' == ref
      _                 -> false