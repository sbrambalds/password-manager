module DataModel.Proxy where

import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Functor (class Functor)
import Data.HexString (HexString)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect.Aff (Fiber)
import Functions.HashCash (TollChallenge)

type PathPrefix = String

defaultPathPrefix :: PathPrefix
defaultPathPrefix = "/api"

type Path = String
type SessionKey = HexString

type BackendSessionState = {
  b  :: HexString
, aa :: HexString
, bb :: HexString
}

type TollManager = {
  toll             :: Maybe (Either (Fiber HexString) HexString)
, currentChallenge :: Maybe  TollChallenge
}

data Proxy = DynamicProxy DynamicProxy
           | StaticProxy (Maybe BackendSessionState)

data DynamicProxy = OnlineProxy PathPrefix TollManager (Maybe SessionKey) | OfflineProxy (Maybe BackendSessionState) DataOnLocalStorage

defaultOnlineProxy :: DynamicProxy
defaultOnlineProxy = OnlineProxy defaultPathPrefix {toll: Nothing, currentChallenge: Nothing} Nothing

type MissingEntries = List HexString

data DataOnLocalStorage = WithData MissingEntries | NoData
derive instance eqDataOnLocalStorage :: Eq DataOnLocalStorage

data ProxyInfo = Static | Online | Offline DataOnLocalStorage
derive instance eqProxyInfo :: Eq ProxyInfo

data ProxyResponse a = ProxyResponse Proxy a

instance proxyResponseFunctor :: Functor ProxyResponse where
  map f (ProxyResponse proxy a) = ProxyResponse proxy (f a)

discardResult :: forall a. ProxyResponse a -> ProxyResponse Unit
discardResult (ProxyResponse proxy _) = ProxyResponse proxy unit

getProxy :: forall a. ProxyResponse a -> Proxy
getProxy (ProxyResponse proxy _) = proxy

responseValue :: forall a. ProxyResponse a -> a
responseValue (ProxyResponse _ a) = a
