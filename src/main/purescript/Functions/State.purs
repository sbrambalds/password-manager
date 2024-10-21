module Functions.State where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire(..))
import Concur.React (HTML)
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (filter, catMaybes, head)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Traversable (fold, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit)
import DataModel.AppState (AppState)
import DataModel.CardVersions.Card (Card)
import DataModel.IndexVersions.Index (Index)
import DataModel.Proxy (DataOnLocalStorage(..), DynamicProxy(..), Proxy(..), ProxyInfo(..), defaultOnlineProxy, defaultPathPrefix)
import DataModel.SRPVersions.SRP (HashFunction, SRPConf, baseSRPConf, hashFuncSHA256)
import DataModel.UserVersions.User (MasterKey, UserInfo, UserInfoReferences)
import Effect (Effect)
import Effect.Class (liftEffect)
import Functions.Donations (DonationLevel)
import OperationalWidgets.Sync (SyncData, SyncOperation(..))
import Record (merge)
import Web.DOM (Element, Node)
import Web.DOM.Element (fromNode, id)
import Web.DOM.Node (childNodes)
import Web.DOM.NodeList (toArray)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toNode)
import Web.HTML.Navigator (onLine)
import Web.HTML.Window (document, navigator)

foreign import _readStaticOfflineCopyTimestamp :: Unit -> String

offlineDataId :: String
offlineDataId = "offlineData"

isStatic :: Effect Boolean
isStatic = isJust <$> runMaybeT do
    body            :: HTMLElement                  <- MaybeT $ (window >>= document >>= body)
    childs          :: Array Node                   <- liftEffect $ (childNodes (toNode body) >>= toArray)
    elementsWithId  :: Array (Tuple Element String) <- liftEffect $ sequence $ mapIds <$> (catMaybes $ fromNode <$> childs)
    MaybeT $ pure $ fst <$> head (filter (\element_id -> (snd element_id) == offlineDataId) elementsWithId)
  
  where 
    mapIds :: Element -> Effect (Tuple Element String)
    mapIds e = (Tuple e) <$> (id e)

getProxyInfoFromProxy :: Proxy -> ProxyInfo
getProxyInfoFromProxy = case _ of
  StaticProxy   _                                  -> Static
  DynamicProxy (OnlineProxy  _ _ _               ) -> Online
  DynamicProxy (OfflineProxy _ dataOnLocalStorage) -> Offline dataOnLocalStorage

computeInitialState :: Wire (Widget HTML) SyncData -> Effect AppState
computeInitialState wire = computeProxy >>= (\proxy -> pure $ merge baseState {proxy, syncDataWire: wire})

  where
    computeProxy :: Effect Proxy
    computeProxy = isStatic >>= case _ of
      true  -> StaticProxy Nothing # pure
      false -> DynamicProxy <$> computeDynamicProxy

    computeDynamicProxy :: Effect DynamicProxy
    computeDynamicProxy = (window >>= navigator >>= onLine) <#> case _ of 
      true  -> OnlineProxy defaultPathPrefix {toll: Nothing, currentChallenge: Nothing} Nothing
      false -> OfflineProxy Nothing NoData

updateProxy :: AppState -> Effect Proxy
updateProxy state@{proxy: DynamicProxy dynamicProxy} = DynamicProxy <$> ((window >>= navigator >>= onLine) >>= case _, state of 
      false, {enableSync: true, syncDataWire} ->
          ((un Wire syncDataWire).value) <#> (\syncData ->
            fold (syncData.pendingOperations <#> (case _ of
              SaveBlobFromRef ref -> ref : Nil
              _                   ->       Nil
            ))
          ) <#>  (preserveOfflineData <<< WithData)
      false, _ -> preserveOfflineData NoData # pure
      true,  _ -> preserveOnlineData         # pure
)
  where
    preserveOfflineData :: DataOnLocalStorage -> DynamicProxy
    preserveOfflineData dataOnLocalStorage = case dynamicProxy of
      OfflineProxy backendSessionState _ -> OfflineProxy backendSessionState dataOnLocalStorage
      _                                  -> OfflineProxy Nothing             dataOnLocalStorage

    preserveOnlineData :: DynamicProxy
    preserveOnlineData = case dynamicProxy of
      OfflineProxy _ _ -> defaultOnlineProxy
      onlineProxy      -> onlineProxy

updateProxy {proxy} = pure proxy


resetState :: AppState -> AppState
resetState state = merge baseState state

baseState ∷ { username :: Maybe String
            , password :: Maybe String
            , pinExists :: Boolean
            , c :: Maybe HexString
            , p :: Maybe HexString
            , s :: Maybe HexString
            , srpConf :: SRPConf
            , hash :: HashFunction
            , cardsCache :: Map HexString Card
            , masterKey :: Maybe MasterKey
            , userInfoReferences :: Maybe UserInfoReferences
            , userInfo :: Maybe UserInfo
            , index :: Maybe Index
            , donationLevel :: Maybe DonationLevel
            , enableSync :: Boolean
            }
baseState = { username: Nothing
            , password: Nothing
            , pinExists: false
            , c: Nothing
            , s: Nothing
            , p: Nothing
            , srpConf: baseSRPConf
            , hash: hashFuncSHA256
            , cardsCache: empty
            , masterKey: Nothing
            , userInfoReferences: Nothing 
            , userInfo: Nothing
            , index: Nothing
            , donationLevel: Nothing
            , enableSync: false
            }
