module DataModel.WidgetState where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML)
import Data.Bounded (class Ord)
import Data.Either (Either)
import Data.Eq (class Eq)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import DataModel.CardVersions.Card (Card)
import DataModel.Credentials (Credentials)
import DataModel.IndexVersions.Index (CardEntry, Index)
import DataModel.Proxy (ProxyInfo)
import DataModel.UserVersions.User (UserPreferences, DonationInfo)
import Functions.Donations (DonationLevel)
import IndexFilterView (FilterData)
import OperationalWidgets.Sync (SyncData)
import Type.Proxy (Proxy(..))
import Views.CreateCardView (CardFormData)
import Views.DeviceSyncView (EnableSync)
import Views.OverlayView (OverlayInfo)
import Views.SignupFormView (SignupDataForm)
import Web.File.File (File)

data Page = Loading | Login | Signup | Main | Donation 
type PagesState = {loading :: Maybe Page, login :: LoginFormData, signup :: SignupDataForm, main :: MainPageWidgetState, donation :: DonationLevel}

_loadingPagesState :: Lens' PagesState (Maybe Page)
_loadingPagesState = prop (Proxy :: _ "loading")

_loginPagesState :: Lens' PagesState LoginFormData
_loginPagesState = prop (Proxy :: _ "login")

_signupPagesState :: Lens' PagesState SignupDataForm
_signupPagesState = prop (Proxy :: _ "signup")

_mainPagesState :: Lens' PagesState MainPageWidgetState
_mainPagesState = prop (Proxy :: _ "main")

_donationPagesState :: Lens' PagesState DonationLevel
_donationPagesState = prop (Proxy :: _ "donation")

-- ========================================================================

type PIN = String

data LoginType = CredentialLogin | PinLogin | PasskeyLogin

type LoginFormData = 
  { credentials :: Credentials
  , pin :: PIN
  , loginType :: LoginType
  }

-- ========================================================================

type UserAreaState = {
  showUserArea     :: Boolean
, userAreaOpenPage :: UserAreaPage
, importState      :: ImportState
, userAreaSubmenus :: Map UserAreaSubmenu Boolean
}

data UserAreaPage = Export | Import | Delete | Preferences | ChangePassword | Pin | Passkey | DeviceSync | Donate | About | None
derive instance eqUserAreaPage :: Eq UserAreaPage

data ImportStep = Upload | Selection | Confirm

type ImportState = {
  step      :: ImportStep
, content   :: Either (Maybe File) String
, selection :: Array (Tuple Boolean Card)
, tag       :: Tuple Boolean String
}

data UserAreaSubmenu = Account | Device | Data
derive instance  eqUserAreaSubmenus :: Eq  UserAreaSubmenu
derive instance ordUserAreaSubmenus :: Ord UserAreaSubmenu

-- ========================================================================

type MainPageWidgetState = {
  index              :: Index
, credentials        :: Credentials
, donationInfo       :: Maybe DonationInfo
, pinExists          :: Boolean
, passkeyExists      :: Boolean
, enableSync         :: EnableSync
, userAreaState      :: UserAreaState
, cardManagerState   :: CardManagerState
, userPreferences    :: UserPreferences
, donationLevel      :: DonationLevel
, syncDataWire       :: Maybe ((Wire (Widget HTML) SyncData))
}

data WidgetState = WidgetState OverlayInfo (Tuple Page PagesState) ProxyInfo

-- -------------------------------------

data CardFormInput = NewCard | NewCardFromFragment Card | ModifyCard Card CardEntry
derive instance eqCardFormInput :: Eq CardFormInput

data CardViewState = NoCard | Card Card CardEntry | CardForm CardFormData CardFormInput
derive instance eqCardViewState :: Eq CardViewState

type CardManagerState = { 
  filterData          :: FilterData
, highlightedEntry    :: Maybe CardEntry
, cardViewState       :: CardViewState
, showShortcutsHelp   :: Boolean
, showDonationOverlay :: Boolean
}