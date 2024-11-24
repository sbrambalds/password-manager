module Test.DebugCodec where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML)
import Data.Argonaut.Core (Json, jsonEmptyString)
import Data.Codec (codec')
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.HexString (hexStringCodec)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, wrapIso)
import Data.Variant as V
import DataModel.CardVersions.Card (Card(..), CardField(..), CardValues(..), cardVersionCodec)
import DataModel.Credentials (Credentials)
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), Index(..))
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.Proxy (DataOnLocalStorage(..), ProxyInfo(..))
import DataModel.UserVersions.User (UserPreferences(..), DonationInfo)
import DataModel.UserVersions.UserCodecs (dateTimeCodec)
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState, ImportState, ImportStep(..), LoginFormData, LoginType(..), MainPageWidgetState, Page(..), UserAreaPage(..), UserAreaState, UserAreaSubmenu(..), WidgetState(..))
import DataModel.WidgetState (CardViewState(..)) as CardViewState
import Functions.Donations (DonationLevel(..))
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..))
import OperationalWidgets.Sync (SyncData)
import Type.Proxy (Proxy(..))
import Views.CreateCardView (CardFormData)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), OverlayInfo)
import Views.SignupFormView (SignupDataForm)
import Web.File.File (File)

-- data WidgetState = WidgetState OverlayInfo Page
widgetStateCodec :: CA.JsonCodec WidgetState
widgetStateCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { widgetState: Right (CA.object "WidgetState" $ CAR.record {overlayInfo: overlayInfoCodec, proxyInfo: proxyInfoCodec, page: pageCodec})
    }
  where
    toVariant = case _ of
      WidgetState oi p pi -> V.inj (Proxy :: _ "widgetState") {page: p, overlayInfo: oi, proxyInfo: pi}
    fromVariant = V.match
      { widgetState: \{page, overlayInfo, proxyInfo} -> WidgetState overlayInfo page proxyInfo
      }

-- type OverlayInfo   = { status :: OverlayStatus, color :: OverlayColor, message :: String }
overlayInfoCodec :: CA.JsonCodec OverlayInfo
overlayInfoCodec =
  CA.object "OverlayInfo"
    (CAR.record
      { status:  overlayStatusCodec
      , color:   overlayColorCodec
      , message: CA.string
      }
    )

-- data OverlayStatus = Hidden | Spinner | Done | Failed | Copy
overlayStatusCodec :: CA.JsonCodec OverlayStatus
overlayStatusCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { hidden:  Left unit
    , spinner: Left unit
    , done:    Left unit
    , failed:  Left unit
    , copy:    Left unit
    }
  where
    toVariant = case _ of
      Hidden  -> V.inj (Proxy :: _ "hidden")  unit
      Spinner -> V.inj (Proxy :: _ "spinner") unit
      Done    -> V.inj (Proxy :: _ "done")    unit
      Failed  -> V.inj (Proxy :: _ "failed")  unit
      Copy    -> V.inj (Proxy :: _ "copy")    unit
    fromVariant = V.match
      { hidden:  \_ -> Hidden
      , spinner: \_ -> Spinner
      , done:    \_ -> Done
      , failed:  \_ -> Failed
      , copy:    \_ -> Copy
      }

-- data OverlayColor  = Black | White
overlayColorCodec :: CA.JsonCodec OverlayColor
overlayColorCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { black: Left unit
    , white: Left unit
    }
  where
    toVariant = case _ of
      Black -> V.inj (Proxy :: _ "black") unit
      White -> V.inj (Proxy :: _ "white") unit
    fromVariant = V.match
      { black:  \_ -> Black
      , white: \_  -> White
      }

-- data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Main MainPageWidgetState | Donation DonationLevel
pageCodec :: CA.JsonCodec Page
pageCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { loading:  Left  Nothing-- Right (CAR.maybe pageCodec)
    , login:    Right loginFormDataCodec
    , signup:   Right signupDataFormCodec
    , main:     Right mainPageWidgetStateCodec
    , donation: Right donationLevelCodec
    }
  where
    toVariant = case _ of
      Loading _   -> V.inj (Proxy :: _ "loading" ) Nothing
      Login   lfd -> V.inj (Proxy :: _ "login"   ) lfd
      Signup  sdf -> V.inj (Proxy :: _ "signup"  ) sdf
      Main   mpws -> V.inj (Proxy :: _ "main"    ) mpws
      Donation dl -> V.inj (Proxy :: _ "donation") dl
    fromVariant = V.match
      { loading:  Loading
      , login:    Login
      , signup:   Signup
      , main:     Main
      , donation: Donation
      }

-- type LoginFormData = 
--   { credentials :: Credentials
--   , pin :: PIN
--   , loginType :: LoginType
--   }
loginFormDataCodec :: CA.JsonCodec LoginFormData
loginFormDataCodec =
  CA.object "LoginFormData"
    (CAR.record
      { credentials: credentialsCodec
      , pin:         CA.string
      , loginType:   loginTypeCodec
      }
    )

-- type Credentials =  { username :: String
--                     , password :: String
--                     }
credentialsCodec :: CA.JsonCodec Credentials
credentialsCodec =
  CA.object "Credentials"
    (CAR.record
      { username: CA.string
      , password: CA.string
      }
    )

-- data LoginType = CredentialLogin | PinLogin
loginTypeCodec :: CA.JsonCodec LoginType
loginTypeCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { credentialsLogin: Left unit
    , pinLogin:         Left unit
    }
  where
    toVariant = case _ of
      CredentialLogin -> V.inj (Proxy :: _ "credentialsLogin") unit
      PinLogin        -> V.inj (Proxy :: _ "pinLogin"        ) unit
    fromVariant = V.match
      { credentialsLogin:  \_ -> CredentialLogin
      , pinLogin:          \_ -> PinLogin
      }

-- type SignupDataForm = { username       :: String
--                       , password       :: String
--                       , verifyPassword :: String
--                       , checkboxes     :: Array (Tuple String Boolean)
--                       }
signupDataFormCodec :: CA.JsonCodec SignupDataForm
signupDataFormCodec =
  CA.object "SignupDataForm"
    (CAR.record
      { username:       CA.string
      , password:       CA.string
      , verifyPassword: CA.string
      , checkboxes:     CA.array (CAC.tuple CA.string CA.boolean)
      }
    )

-- type DonationInfo = {
--   dateOfLastDonation   :: DateTime
-- , nextDonationReminder :: DateTime
-- }

donationInfoCodec :: CA.JsonCodec DonationInfo
donationInfoCodec = 
  CAR.object "DonationInfo"
    { dateOfLastDonation: dateTimeCodec
    , nextDonationReminder: dateTimeCodec
    }

-- data DataOnLocalStorage = WithData (List CardEntry) | NoData
dataOnLocalStorageCodec :: CA.JsonCodec DataOnLocalStorage
dataOnLocalStorageCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { withData : Right (CAC.list hexStringCodec)
    , noData   : Left unit
    }
  where
    toVariant = case _ of
      WithData missing -> V.inj (Proxy :: _ "withData") missing
      NoData           -> V.inj (Proxy :: _ "noData"  ) unit
    fromVariant = V.match
      { withData:       WithData
      , noData  : \_ -> NoData
      }

-- data ProxyInfo = Static | Online | Offline DataOnLocalStorage
proxyInfoCodec :: CA.JsonCodec ProxyInfo
proxyInfoCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { static: Left unit
    , online: Left unit
    , offline: Right dataOnLocalStorageCodec
    }
  where
    toVariant = case _ of
      Static       -> V.inj (Proxy :: _ "static" ) unit
      Online       -> V.inj (Proxy :: _ "online" ) unit
      Offline dols -> V.inj (Proxy :: _ "offline") dols
    fromVariant = V.match
      { static:  \_ -> Static
      , online:  \_ -> Online
      , offline:       Offline
      }

-- type MainPageWidgetState = {
--   index                         :: Index
-- , credentials                   :: Credentials
-- , donationInfo                  :: Maybe DonationInfo
-- , pinExists                     :: Boolean
-- , userAreaState                 :: UserAreaState
-- , cardManagerState              :: CardManagerState
-- , userPreferences               :: UserPreferences
-- , donationLevel :: DonationLevel
-- , enableSync :: Boolean
-- , syncDataWire :: Maybe ((Wire (Widget HTML) SyncData))
-- }
mainPageWidgetStateCodec :: CA.JsonCodec MainPageWidgetState
mainPageWidgetStateCodec = 
  CA.object "MainPageWidgetState"
    (CAR.record
      { index:            indexCodec
      , credentials:      credentialsCodec
      , donationInfo:     CAC.maybe donationInfoCodec
      , pinExists:        CA.boolean
      , userAreaState:    userAreaStateCodec
      , cardManagerState: cardManagerStateCodec
      , userPreferences:  userPreferencesCodec
      , donationLevel:    donationLevelCodec
      , enableSync:       CA.boolean
      , syncDataWire:     syncDataWireCodec
      }
    )

syncDataWireCodec :: CA.JsonCodec (Maybe ((Wire (Widget HTML) SyncData)))
syncDataWireCodec = codec' (\(_ :: Json) -> Right Nothing) (\(_ :: Maybe ((Wire (Widget HTML) SyncData))) -> jsonEmptyString)

-- newtype Index = 
--   Index {entries :: (List CardEntry), identifier :: HexString}
indexCodec :: CA.JsonCodec Index
indexCodec =
  wrapIso Index $
    CAR.object "index"
    { entries: CAC.list cardEntryCodec
    , identifier: hexStringCodec
    }

-- newtype CardEntry =
--   CardEntry
--     { title :: String
--     , cardReference :: CardReference
--     , archived :: Boolean
--     , tags :: Array String
--     , lastUsed :: Number
--     -- , attachment :: Boolean
--     }
cardEntryCodec :: CA.JsonCodec CardEntry
cardEntryCodec = wrapIso CardEntry $
  CA.object "CardEntry"
    (CAR.record
      { title:         CA.string
      , cardReference: cardReferenceCodec
      , archived:      CA.boolean
      , tags:          CAC.set CA.string
      , lastUsed:      CA.number
      }
    )

-- newtype CardReference =
--   CardReference
--     { reference :: HexString
--     , key :: HexString
--     , version :: CardVersion
--     , identifier :: HexString
--     }
cardReferenceCodec :: CA.JsonCodec CardReference
cardReferenceCodec = wrapIso CardReference $
  CA.object "CardReference"
    (CAR.record
      { reference:   hexStringCodec
      , key:         hexStringCodec
      , version:     cardVersionCodec
      , identifier:  hexStringCodec
      }
    )

-- type UserAreaState = {
--   showUserArea     :: Boolean
-- , userAreaOpenPage :: UserAreaPage
-- , importState      :: ImportState
-- , userAreaSubmenus :: Map UserAreaSubmenu Boolean
-- }
userAreaStateCodec :: CA.JsonCodec UserAreaState
userAreaStateCodec =
  CA.object "UserAreaState"
    (CAR.record
      { showUserArea     : CA.boolean
      , userAreaOpenPage : userAreaPageCodec
      , importState      : importStateCodec
      , userAreaSubmenus : CAC.map userAreaSubmenuCodec CA.boolean
      }
    )

-- data UserAreaPage = Export | Import | Pin | LocalSync | Delete | Preferences | Donate | ChangePassword | About | None
userAreaPageCodec :: CA.JsonCodec UserAreaPage
userAreaPageCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { export:         Left unit
    , import:         Left unit
    , pin:            Left unit
    , delete:         Left unit
    , preferences:    Left unit
    , changePassword: Left unit
    , donate:         Left unit
    , about:          Left unit
    , noPage:         Left unit
    , deviceSync:     Left unit
    }
  where
    toVariant = case _ of
      Export         -> V.inj (Proxy :: _ "export") unit
      Import         -> V.inj (Proxy :: _ "import") unit
      Pin            -> V.inj (Proxy :: _ "pin") unit
      Delete         -> V.inj (Proxy :: _ "delete") unit
      Preferences    -> V.inj (Proxy :: _ "preferences") unit
      ChangePassword -> V.inj (Proxy :: _ "changePassword") unit
      Donate         -> V.inj (Proxy :: _ "donate") unit
      About          -> V.inj (Proxy :: _ "about") unit
      None           -> V.inj (Proxy :: _ "noPage") unit
      DeviceSync     -> V.inj (Proxy :: _ "deviceSync") unit
    fromVariant = V.match
      { export:         \_ -> Export
      , import:         \_ -> Import
      , pin:            \_ -> Pin
      , delete:         \_ -> Delete
      , preferences:    \_ -> Preferences
      , changePassword: \_ -> ChangePassword
      , donate:         \_ -> Donate
      , about:          \_ -> About
      , noPage:         \_ -> None
      , deviceSync:     \_ -> DeviceSync
      }


-- type ImportState = {
--   step      :: ImportStep
-- , content   :: Either (Maybe File) String
-- , selection :: Array (Tuple Boolean Card)
-- , tag       :: Tuple Boolean String
-- }
importStateCodec :: CA.JsonCodec ImportState
importStateCodec = 
  CA.object "ImportState"
    (CAR.record
      { step      : importStepCodec
      , content   : CAC.either (maybeFileCodec) CA.string
      , selection : CA.array (CAC.tuple CA.boolean cardCodec)
      , tag       : CAC.tuple CA.boolean CA.string
      }
    )
  where
    maybeFileCodec :: CA.JsonCodec (Maybe File)
    maybeFileCodec = dimap toVariant fromVariant $ CAV.variantMatch
        { file: Left unit }
      where
        toVariant   _ = V.inj (Proxy :: _ "file") unit
        fromVariant   = V.match {file: \_ -> Nothing}

-- data ImportStep = Upload | Selection | Confirm
importStepCodec :: CA.JsonCodec ImportStep
importStepCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { upload:    Left unit
    , selection: Left unit
    , confirm:   Left unit
    }
  where
    toVariant = case _ of
      Upload    -> V.inj (Proxy :: _ "upload"   ) unit
      Selection -> V.inj (Proxy :: _ "selection") unit
      Confirm   -> V.inj (Proxy :: _ "confirm"  ) unit    
    fromVariant = V.match
      { upload:    \_ -> Upload
      , selection: \_ -> Selection
      , confirm:   \_ -> Confirm
      }

-- data UserAreaSubmenu = Account | Device | Data
userAreaSubmenuCodec :: CA.JsonCodec UserAreaSubmenu
userAreaSubmenuCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { account: Left unit
    , device:  Left unit
    , data:    Left unit
    }
  where
    toVariant = case _ of
      Account -> V.inj (Proxy :: _ "account") unit
      Device  -> V.inj (Proxy :: _ "device" ) unit
      Data    -> V.inj (Proxy :: _ "data"   ) unit
    fromVariant = V.match
      { account: \_ -> Account
      , device:  \_ -> Device
      , data:    \_ -> Data
      }

-- newtype Card = 
--   Card 
--     { content :: CardValues
--     , secrets :: Array String
--     , archived :: Boolean
--     , timestamp :: Number
--     }
cardCodec :: CA.JsonCodec Card
cardCodec = wrapIso Card (
  CA.object "Card"
    (CAR.record
      { content:   cardValuesCodec
      , secrets:   CA.array CA.string
      , archived:  CA.boolean
      , timestamp: CA.number
      }
    )
)

-- newtype CardValues = 
--   CardValues
--     { title   :: String
--     , tags    :: Array String
--     , fields  :: Array CardField
--     , notes   :: String
--     }
cardValuesCodec :: CA.JsonCodec CardValues
cardValuesCodec = wrapIso CardValues (
  CA.object "CardValues"
    (CAR.record
      { title  : CA.string
      , tags   : CAC.set CA.string
      , fields : CA.array cardFieldCodec
      , notes  : CA.string
      }
    )
)

-- newtype CardField =
--   CardField
--     { name   :: String
--     , value  :: String
--     , locked :: Boolean
--     , settings :: Maybe PasswordGeneratorSettings
--     }
cardFieldCodec :: CA.JsonCodec CardField
cardFieldCodec = wrapIso CardField (
  CA.object "CardField"
    (CAR.record
      { name     : CA.string
      , value    : CA.string
      , locked   : CA.boolean
      , settings : CAR.optional passwordGeneratorSettingsCodec
      }
    )
)

-- type PasswordGeneratorSettings = {
--     length              :: Int,
--     characters          :: String
-- }
passwordGeneratorSettingsCodec :: CA.JsonCodec PasswordGeneratorSettings
passwordGeneratorSettingsCodec = 
  CA.object "PasswordGeneratorSettings"
    (CAR.record
      { length     : CA.int
      , characters : CA.string
      }
    )

-- type CardManagerState = { 
--   filterData          :: FilterData
-- , highlightedEntry    :: Maybe CardEntry
-- , cardViewState       :: CardViewState
-- , showShortcutsHelp   :: Boolean
-- , showDonationOverlay :: Boolean
-- }
cardManagerStateCodec :: CA.JsonCodec CardManagerState
cardManagerStateCodec =
  CA.object "CardManagerState"
    (CAR.record
      { filterData         : filterDataCodec
      , highlightedEntry   : CAR.optional cardEntryCodec
      , cardViewState      : cardViewStateCodec
      , showShortcutsHelp  : CA.boolean
      , showDonationOverlay: CA.boolean
      }
    )

-- type FilterData = { 
--   archived :: Boolean
-- , filter :: Filter
-- , filterViewStatus :: FilterViewStatus
-- , searchString :: String
-- , isFocused :: Boolean
-- }
filterDataCodec :: CA.JsonCodec FilterData
filterDataCodec =
  CA.object "FilterData"
    (CAR.record
      { archived:         CA.boolean
      , filter:           filterCodec
      , filterViewStatus: filterViewStatusCodec
      , searchString:     CA.string
      , selected:         CA.boolean
      }
    )

-- data Filter = All | Recent | Untagged | Search String | Tag String
filterCodec :: CA.JsonCodec Filter
filterCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { all:      Left unit
    , recent:   Left unit
    , untagged: Left unit
    , search:   Right CA.string
    , tag:      Right CA.string
    }
  where
    toVariant = case _ of
      All      -> V.inj (Proxy :: _ "all"     ) unit
      Recent   -> V.inj (Proxy :: _ "recent"  ) unit
      Untagged -> V.inj (Proxy :: _ "untagged") unit
      Search s -> V.inj (Proxy :: _ "search"  ) s
      Tag s    -> V.inj (Proxy :: _ "tag"     ) s
    fromVariant = V.match
      { all:      \_ -> All
      , recent:   \_ -> Recent
      , untagged: \_ -> Untagged
      , search:   Search
      , tag:      Tag
      }

-- data FilterViewStatus = FilterViewClosed | FilterViewOpen
filterViewStatusCodec :: CA.JsonCodec FilterViewStatus
filterViewStatusCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { close: Left unit
    , open:  Left unit
    }
  where
    toVariant = case _ of
      FilterViewClosed -> V.inj (Proxy :: _ "close") unit
      FilterViewOpen   -> V.inj (Proxy :: _ "open" ) unit
    fromVariant = V.match
      { close: \_ -> FilterViewClosed
      , open:  \_ -> FilterViewOpen
      }

-- data CardViewState = NoCard | Card Card CardEntry | CardForm CardFormInput 
cardViewStateCodec :: CA.JsonCodec CardViewState
cardViewStateCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { noCard:   Left   unit
    , card:     Right (CAR.object "Card" {card: cardCodec, cardEntry: cardEntryCodec })
    , cardForm: Right (CAR.object "CardForm" {cardFormData: cardFormDataCodec, cardFormInput: cardFormInputCodec})
    }
  where
    toVariant = case _ of
      CardViewState.NoCard           -> V.inj (Proxy :: _ "noCard"  ) unit
      CardViewState.Card c ce        -> V.inj (Proxy :: _ "card"    ) {card: c, cardEntry: ce}
      CardViewState.CardForm cfd cfi -> V.inj (Proxy :: _ "cardForm") {cardFormData: cfd, cardFormInput: cfi}
    fromVariant = V.match
      { noCard:   \_                             -> CardViewState.NoCard
      , card:     \{card, cardEntry}             -> CardViewState.Card card cardEntry
      , cardForm: \{cardFormData, cardFormInput} -> CardViewState.CardForm cardFormData cardFormInput
      }

-- data CardFormInput = NewCard | NewCardFromFragment Card | ModifyCard Card CardEntry
cardFormInputCodec :: CA.JsonCodec CardFormInput
cardFormInputCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { newCard             : Left   unit
    , newCardFromFragment : Right  cardCodec
    , modifyCard          : Right (CA.object "ModifyCard" (CAR.record {card: cardCodec, cardEntry: cardEntryCodec }))
    }
  where
    toVariant = case _ of
      NewCard                -> V.inj (Proxy :: _ "newCard"            )  unit
      NewCardFromFragment ce -> V.inj (Proxy :: _ "newCardFromFragment")  ce
      ModifyCard c ce        -> V.inj (Proxy :: _ "modifyCard"         ) {card: c, cardEntry: ce}
    fromVariant = V.match
      { newCard:              \_ -> NewCard
      , newCardFromFragment: (\cardEntry         -> NewCardFromFragment cardEntry)
      , modifyCard:          (\{card, cardEntry} -> ModifyCard card cardEntry)
      }

-- type CardFormData = {
--   newTag  :: String
-- , preview :: Boolean
-- , card    :: Card
-- }
cardFormDataCodec :: CA.JsonCodec CardFormData
cardFormDataCodec = CAR.object "CardFormData"
  { newTag: CA.string
  , preview: CA.boolean
  , card: cardCodec
  }

-- newtype UserPreferences = 
--   UserPreferences
--     { passwordGeneratorSettings :: PasswordGeneratorSettings
--     , automaticLock :: Either Int Int -- Left  -> automatic lock disabled while keeping the time
--                                       -- Right -> automatic lock enabled
--     }
userPreferencesCodec :: CA.JsonCodec UserPreferences
userPreferencesCodec = wrapIso UserPreferences (
  CA.object "UserPreferences"
    (CAR.record
      { passwordGeneratorSettings: passwordGeneratorSettingsCodec
      , automaticLock:             CAC.either CA.int CA.int
      }
    )
)

-- data DonationLevel = DonationOk | DonationInfo | DonationWarning
donationLevelCodec :: CA.JsonCodec DonationLevel
donationLevelCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { donationOk      : Left unit
    , donationInfo    : Left unit
    , donationWarning : Left unit
    }
  where
    toVariant = case _ of
      DonationOk      -> V.inj (Proxy :: _ "donationOk"     ) unit
      DonationInfo    -> V.inj (Proxy :: _ "donationInfo"   ) unit
      DonationWarning -> V.inj (Proxy :: _ "donationWarning") unit
    fromVariant = V.match
      { donationOk:      \_ -> DonationOk
      , donationInfo:    \_ -> DonationInfo
      , donationWarning: \_ -> DonationWarning
      }
