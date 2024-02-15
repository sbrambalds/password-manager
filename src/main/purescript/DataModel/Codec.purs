module DataModel.Codec where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (HexString(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, wrapIso)
import Data.Unit (unit)
import Data.Variant as V
import DataModel.Card (Card(..), CardField(..), CardValues(..))
import DataModel.CardVersions.CardV1 (CardValues_V1, Card_V1, CardField_V1)
import DataModel.Credentials (Credentials)
import DataModel.Index (CardEntry(..), CardReference(..), Index(..))
import DataModel.IndexVersions.IndexV1 (CardEntry_V1, CardReference_V1, Index_V1)
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.Pin (PasswordPin)
import DataModel.User (IndexReference(..), UserInfoReferences(..), UserPreferences(..), UserPreferencesReference(..))
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState, ImportState, ImportStep(..), LoginFormData, LoginType(..), MainPageWidgetState, Page(..), UserAreaPage(..), UserAreaState, UserAreaSubmenu(..), WidgetState(..))
import DataModel.WidgetState (CardViewState(..)) as CardViewState
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..))
import Type.Proxy (Proxy(..))
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), OverlayInfo)
import Views.SignupFormView (SignupDataForm)
import Web.File.File (File)

-- data WidgetState = WidgetState OverlayInfo Page
widgetStateCodec :: CA.JsonCodec WidgetState
widgetStateCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { widgetState: Right (CA.object "WidgetState" $ CAR.record {overlayInfo: overlayInfoCodec, page: pageCodec})
    }
  where
    toVariant = case _ of
      WidgetState oi p -> V.inj (Proxy :: _ "widgetState") {page: p, overlayInfo: oi}
    fromVariant = V.match
      { widgetState: \{page, overlayInfo} -> WidgetState overlayInfo page
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


-- data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Main MainPageWidgetState
pageCodec :: CA.JsonCodec Page
pageCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { loading: Left  Nothing-- Right (CAR.maybe pageCodec)
    , login:   Right loginFormDataCodec
    , signup:  Right signupDataFormCodec
    , main:    Right mainPageWidgetStateCodec
    }
  where
    toVariant = case _ of
      Loading _   -> V.inj (Proxy :: _ "loading") Nothing
      Login   lfd -> V.inj (Proxy :: _ "login"  ) lfd
      Signup  sdf -> V.inj (Proxy :: _ "signup" ) sdf
      Main   mpws -> V.inj (Proxy :: _ "main"   ) mpws
    fromVariant = V.match
      { loading: Loading
      , login:   Login
      , signup:  Signup
      , main:    Main
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

-- type MainPageWidgetState = {
--   index                         :: Index
-- , credentials                   :: Credentials
-- , pinExists                     :: Boolean
-- , userAreaState                 :: UserAreaState
-- , cardManagerState              :: CardManagerState
-- , userPreferences               :: UserPreferences
-- }
mainPageWidgetStateCodec :: CA.JsonCodec MainPageWidgetState
mainPageWidgetStateCodec = 
  CA.object "MainPageWidgetState"
    (CAR.record
      { index:            indexCodec
      , credentials:      credentialsCodec
      , pinExists:        CA.boolean
      , userAreaState:    userAreaStateCodec
      , cardManagerState: cardManagerStateCodec
      , userPreferences:  userPreferencesCodec
      }
    )

-- newtype Index = 
--   Index (List CardEntry)
indexCodec :: CA.JsonCodec Index
indexCodec =
  wrapIso Index (CAC.list cardEntryCodec)

-- type Index_V1 = List CardEntry_V1
indexV1Codec :: CA.JsonCodec Index_V1
indexV1Codec = CAC.list cardEntryV1Codec

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
      , tags:          CA.array CA.string
      , lastUsed:      CA.number
      }
    )
  
-- type CardEntry_V1 = 
--   { title :: String
--   , cardReference :: CardReference_V1
--   , archived :: Boolean
--   , tags :: Array String
--   , lastUsed :: Number
--   }
cardEntryV1Codec :: CA.JsonCodec CardEntry_V1
cardEntryV1Codec = 
  CA.object "CardEntry"
    (CAR.record
      { title:         CA.string
      , cardReference: cardReferenceV1Codec
      , archived:      CA.boolean
      , tags:          CA.array CA.string
      , lastUsed:      CA.number
      }
    )

-- newtype CardReference =
--   CardReference
--     { reference :: HexString
--     , key :: HexString
--     , cardVersion :: String
--     }
cardReferenceCodec :: CA.JsonCodec CardReference
cardReferenceCodec = wrapIso CardReference $
  CA.object "CardReference"
    (CAR.record
      { reference:   hexStringCodec
      , key:         hexStringCodec
      , cardVersion: CA.string
      }
    )

-- type CardReference_V1 =
--   { reference :: HexString
--   , key :: HexString
--   , cardVersion :: String
--   }
cardReferenceV1Codec :: CA.JsonCodec CardReference_V1
cardReferenceV1Codec = 
  CA.object "CardReference"
    (CAR.record
      { reference:   hexStringCodec
      , key:         hexStringCodec
      , cardVersion: CA.string
      }
    )

-- newtype HexString = HexString String
hexStringCodec :: CA.JsonCodec HexString
hexStringCodec = wrapIso HexString CA.string

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

-- data UserAreaPage = Export | Import | Pin | Delete | Preferences | ChangePassword | About | None
userAreaPageCodec :: CA.JsonCodec UserAreaPage
userAreaPageCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { export:         Left unit
    , import:         Left unit
    , pin:            Left unit
    , delete:         Left unit
    , preferences:    Left unit
    , changePassword: Left unit
    , about:          Left unit
    , noPage:         Left unit
    }
  where
    toVariant = case _ of
      Export         -> V.inj (Proxy :: _ "export") unit
      Import         -> V.inj (Proxy :: _ "import") unit
      Pin            -> V.inj (Proxy :: _ "pin") unit
      Delete         -> V.inj (Proxy :: _ "delete") unit
      Preferences    -> V.inj (Proxy :: _ "preferences") unit
      ChangePassword -> V.inj (Proxy :: _ "changePassword") unit
      About          -> V.inj (Proxy :: _ "about") unit
      None           -> V.inj (Proxy :: _ "noPage") unit     
    fromVariant = V.match
      { export:         \_ -> Export
      , import:         \_ -> Import
      , pin:            \_ -> Pin
      , delete:         \_ -> Delete
      , preferences:    \_ -> Preferences
      , changePassword: \_ -> ChangePassword
      , about:          \_ -> About
      , noPage:         \_ -> None
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

-- data UserAreaSubmenu = Account | Data
userAreaSubmenuCodec :: CA.JsonCodec UserAreaSubmenu
userAreaSubmenuCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { account: Left unit
    , data:    Left unit
    }
  where
    toVariant = case _ of
      Account -> V.inj (Proxy :: _ "account") unit
      Data    -> V.inj (Proxy :: _ "data"   ) unit
    fromVariant = V.match
      { account: \_ -> Account
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

-- type Card_V1 = 
--   { content :: CardValues_V1
--   , secrets :: Array String
--   , archived :: Boolean
--   , timestamp :: Number
--   }
cardV1Codec :: CA.JsonCodec Card_V1
cardV1Codec =
  CAR.object "cardV1"
    { content   : cardValuesV1Codec
    , secrets   : CA.array CA.string
    , archived  : CA.boolean
    , timestamp : CA.number
    }

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
      , tags   : CA.array CA.string
      , fields : CA.array cardFieldCodec
      , notes  : CA.string
      }
    )
)

-- type CardValues_V1 = 
--   { title   :: String
--   , tags    :: Array String
--   , fields  :: Array CardField_V1
--   , notes   :: String
--   }
cardValuesV1Codec :: CA.JsonCodec CardValues_V1
cardValuesV1Codec = 
  CAR.object "cardValuesV1"
    { title   : CA.string
    , tags    : CA.array CA.string
    , fields  : CA.array cardFieldV1Codec
    , notes   : CA.string
    }

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

-- type CardField_V1 =
--   { name   :: String
--   , value  :: String
--   , locked :: Boolean
--   , settings :: Maybe PasswordGeneratorSettings
--   }
cardFieldV1Codec :: CA.JsonCodec CardField_V1
cardFieldV1Codec =
  CAR.object "cardFieldV1"
    { name     : CA.string
    , value    : CA.string
    , locked   : CA.boolean
    , settings : CAR.optional passwordGeneratorSettingsCodec
    }

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
--   filterData    :: FilterData
-- , selectedEntry :: Maybe CardEntry
-- , cardViewState :: CardViewState
-- }
cardManagerStateCodec :: CA.JsonCodec CardManagerState
cardManagerStateCodec =
  CA.object "CardManagerState"
    (CAR.record
      { filterData    : filterDataCodec
      , selectedEntry : CAR.optional cardEntryCodec
      , cardViewState : cardViewStateCodec
      }
    )

-- type FilterData = { 
--   archived :: Boolean
-- , filter :: Filter
-- , filterViewStatus :: FilterViewStatus
-- , searchString :: String
-- }
filterDataCodec :: CA.JsonCodec FilterData
filterDataCodec =
  CA.object "FilterData"
    (CAR.record
      { archived:         CA.boolean
      , filter:           filterCodec
      , filterViewStatus: filterViewStatusCodec
      , searchString:     CA.string
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
    { noCard:   Left  unit
    , card:     Right (CA.object "Card" (CAR.record {card: cardCodec, cardEntry: cardEntryCodec }))
    , cardForm: Right cardFormInputCodec
    }
  where
    toVariant = case _ of
      CardViewState.NoCard       -> V.inj (Proxy :: _ "noCard"  ) unit
      CardViewState.Card c ce    -> V.inj (Proxy :: _ "card"    ) {card: c, cardEntry: ce}
      CardViewState.CardForm cfi -> V.inj (Proxy :: _ "cardForm") cfi
    fromVariant = V.match
      { noCard:   \_                 -> CardViewState.NoCard
      , card:     \{card, cardEntry} -> CardViewState.Card card cardEntry
      , cardForm:                       CardViewState.CardForm
      }

-- data CardFormInput = NewCard (Maybe Card) | ModifyCard Card CardEntry
cardFormInputCodec :: CA.JsonCodec CardFormInput
cardFormInputCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { newCard:    Right (CAC.maybe cardCodec)
    , modifyCard: Right (CA.object "ModifyCard" (CAR.record {card: cardCodec, cardEntry: cardEntryCodec }))
    }
  where
    toVariant = case _ of
      NewCard mc      -> V.inj (Proxy :: _ "newCard"     ) mc
      ModifyCard c ce -> V.inj (Proxy :: _ "modifyCard" ) {card: c, cardEntry: ce}
    fromVariant = V.match
      { newCard:    NewCard
      , modifyCard: (\{card, cardEntry} -> ModifyCard card cardEntry)
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

-- newtype UserInfoReferences = --TODO: change references structure [fsolaroli - 06/01/2024]
--   UserInfoReferences 
--     { preferencesReference :: UserPreferencesReference
--     , indexReference :: IndexReference
--     }
userInfoReferencesCodec :: CA.JsonCodec UserInfoReferences
userInfoReferencesCodec = wrapIso UserInfoReferences $
  CA.object "UserInfoReferences"
    (CAR.record
      { preferencesReference: userPreferencesReferenceCodec
      , indexReference:       indexReferenceCodec
      }
    )

-- newtype UserPreferencesReference =
--   UserPreferencesReference
--     { reference :: HexString
--     , key :: HexString
--     }
userPreferencesReferenceCodec :: CA.JsonCodec UserPreferencesReference
userPreferencesReferenceCodec = wrapIso UserPreferencesReference $
  CA.object "UserPreferencesReference"
    (CAR.record
      { reference: hexStringCodec
      , key:       hexStringCodec
      }
    )

-- newtype IndexReference =
--   IndexReference
--     { reference :: HexString
--     , masterKey :: HexString
--     , indexVersion :: String
--     }
indexReferenceCodec :: CA.JsonCodec IndexReference
indexReferenceCodec = wrapIso IndexReference $
  CA.object "IndexReference"
    (CAR.record
      { reference:    hexStringCodec
      , masterKey:    hexStringCodec
      , indexVersion: CA.string
      }
    )

-- type PasswordPin = { padding :: Int, passphrase :: String }
passwordPinCodec :: CA.JsonCodec PasswordPin
passwordPinCodec =
  CAR.object "passwordPin" { padding : CA.int , passphrase : CA.string }