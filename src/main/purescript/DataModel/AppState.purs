module DataModel.AppState where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML)
import Data.HexString (HexString)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe)
import DataModel.CardVersions.Card (Card)
import DataModel.IndexVersions.Index (Index)
import DataModel.Proxy (Proxy)
import DataModel.SRPVersions.SRP (HashFunction, SRPConf)
import DataModel.UserVersions.User (MasterKey, UserInfo, UserInfoReferences)
import Functions.Donations (DonationLevel)
import OperationalWidgets.Sync (SyncData)
import Views.DeviceSyncView (EnableSync)

type CardsCache = Map HexString Card

type AppState =
  { proxy :: Proxy
  , username :: Maybe String
  , password :: Maybe String
  , pinExists :: Boolean
  , c :: Maybe HexString
  , p :: Maybe HexString
  , s :: Maybe HexString
  , srpConf :: SRPConf
  , hash :: HashFunction
  , cardsCache :: CardsCache
  , masterKey :: Maybe MasterKey
  , userInfoReferences :: Maybe UserInfoReferences
  , userInfo :: Maybe UserInfo
  , donationLevel :: Maybe DonationLevel
  , index :: Maybe Index
  , syncDataWire :: Wire (Widget HTML) SyncData
  , enableSync :: EnableSync
  }

data AppStateResponse a = AppStateResponse AppState a
