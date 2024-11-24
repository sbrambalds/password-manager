module Views.CardViews where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, fireOnce, loopW)
import Concur.React (HTML, affAction)
import Concur.React.DOM (a, a_, button, div, h3, h4, li', li_, p_, text, textarea, ul)
import Concur.React.Props as Props
import Control.Alt (($>), (<|>))
import Control.Alternative (empty, (*>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Array (null)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not, (&&))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Set (isEmpty, toUnfoldable)
import Data.Unit (unit)
import DataModel.CardVersions.Card (Card(..), CardField(..), CardValues(..), FieldType(..))
import DataModel.IndexVersions.Index (CardEntry)
import DataModel.Proxy (ProxyInfo(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.Card (getFieldType)
import Functions.Clipboard (copyToClipboard)
import Functions.Timer (resetTimer)
import MarkdownIt (renderString)
import Views.Components (dynamicWrapper, entropyMeter)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), overlay)
import Views.SimpleWebComponents (simpleButton, confirmationWidget)

-- -----------------------------------

data CardEvent = Edit    CardEntry Card
               | Clone   CardEntry
               | Archive CardEntry
               | Restore CardEntry
               | Delete  CardEntry 
               | Exit

-- -----------------------------------

cardView :: ProxyInfo -> Card -> CardEntry -> Widget HTML CardEvent
cardView proxyInfo card@(Card r) cardEntry = do
  res <- div [Props._id "cardView"] [
    cardActions (proxyInfo == Online)
  , cardContent r.content
  ]
  case res of
    Delete _ -> do
      confirmation <- div [Props._id "cardView"] [
        false <$ cardActions false
      , cardContent r.content
      , confirmationWidget "Are you sure you want to delete this card?"
      ]
      if confirmation then pure res else cardView proxyInfo card cardEntry
    _ -> pure res

  where
    cardActions :: Boolean -> Widget HTML CardEvent
    cardActions enabled = div [Props.className "cardActions"] [
        simpleButton   "exit"    "exit"     false        (Exit                  )
      , simpleButton   "edit"    "edit"    (not enabled) (Edit    cardEntry card)
      , simpleButton   "clone"   "clone"   (not enabled) (Clone   cardEntry     )
      , if r.archived then
          simpleButton "restore" "restore" (not enabled) (Restore cardEntry     )
        else
          simpleButton "archive" "archive" (not enabled) (Archive cardEntry     )
      , simpleButton   "delete"  "delete"  (not enabled) (Delete  cardEntry     )
    ]

type SecretIdInfo = { creationDate   :: String
                    , expirationDate :: String
                    , secretId       :: String
                    }

secretSignal :: SecretIdInfo -> Signal HTML (Maybe String)
secretSignal { creationDate, expirationDate, secretId } = li_ [] do
  let redeemURL = "/redeem_index.html#" <> secretId
  _ <- a_ [Props.href redeemURL, Props.target "_blank"] (loopW creationDate text)
  _ <- p_ [] (loopW expirationDate text)
  removeSecret <- fireOnce $ simpleButton "remove" "remove secret" false unit
  case removeSecret of
    Nothing -> pure $ Just secretId
    Just _  -> pure $ Nothing

cardContent :: forall a. CardValues -> Widget HTML a
cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) = div [Props._id "cardContent"] [
  h3  [Props.className "card_title"]  [text t]
, if (isEmpty ts) then (empty) else div [Props.className "card_tags"] [ul [] $ (\s -> li' [text s]) <$> (toUnfoldable ts)]
, if (null    fs) then (empty) else div [Props.className "card_fields"] $ cardField false <$> fs
, div [Props.className "card_notes"] [
    if (isEmpty ts && null fs) then (empty) else h4 [] [text "Notes"]
  , div [Props.className "markdown-body", Props.dangerouslySetInnerHTML { __html: unsafePerformEffect $ renderString n}] []
  ]
]

data CardFieldAction = ShowPassword | HidePassword | CopyValue

cardField :: forall a. Boolean -> CardField -> Widget HTML a
cardField showPassword f@(CardField {name, value, locked}) = do
  res <- div [Props.className "cardField"] [
    div [Props.className "fieldValues"] [
      div [Props.className "fieldLabel"] [text name]
    , div [Props.className "fieldValue", Props.onClick $> CopyValue] [
        dynamicWrapper (if (locked && (not showPassword)) then Just "PASSWORD" else Nothing) value $ textarea [Props.rows 1, Props.value value, Props.readOnly true] [] 
      ]
    , if locked
      then (entropyMeter value)
      else  empty
    ]
  , div [Props.className "fieldAction"] [
      getActionButton
    ]
  ]
  (case res of
    ShowPassword -> cardField true         f <|> (affAction $                          delay (Milliseconds 5000.0))
    CopyValue    -> cardField showPassword f <|> (affAction $ copyToClipboard value *> delay (Milliseconds 1000.0)) <|> overlay { status: Copy, color: Black, message: "copied" }
    HidePassword -> pure unit
  *> (resetTimer # liftEffect))
  cardField false f

  where
    getActionButton =
      case getFieldType f of
        Passphrase  -> button [Props.className "action PASSWORD", Props.disabled false, Props.onClick        ] [text "view password"] $> if showPassword then HidePassword else ShowPassword
        Email       -> button [Props.className "action EMAIL",    Props.disabled true                        ] [text "email"        ]
        Url         -> a      [Props.className "action URL",      Props.disabled false, Props.href   value
                                                                                      , Props.target "_blank"] [text "url"          ]
        None        -> button [Props.className "action NONE",     Props.disabled true                        ] [text "none"         ]

