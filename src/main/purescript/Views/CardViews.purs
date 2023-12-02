module Views.CardViews where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, fireOnce, loopW)
import Concur.React (HTML)
import Concur.React.DOM (a_, div, h3, li', li_, p_, text, textarea, ul)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array (null)
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not, (&&))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Unit (unit)
import DataModel.Card (Card(..), CardField(..), CardValues(..))
import Effect.Unsafe (unsafePerformEffect)
import Functions.Clipboard (copyToClipboard)
import MarkdownIt (renderString)
import Views.Components (dynamicWrapper, entropyMeter)
import Views.SimpleWebComponents (simpleButton, confirmationWidget)

-- -----------------------------------

data CardEvent = Edit    Card
               | Clone   Card
               | Archive Card
               | Restore Card
               | Delete  Card
               | Used    Card
               | Exit    Card
               | Share   Card

-- -----------------------------------

cardView :: Card -> Widget HTML CardEvent
cardView card@(Card r) = do
  res <- div [Props._id "cardView"] [
    cardActions card true
  , (Used card) <$ cardContent r.content
  ]
  case res of
    Delete _ -> do
      confirmation <- div [Props._id "cardView"] [
        false <$ cardActions card false
      , cardContent r.content
      , confirmationWidget "Are you sure you want to delete this card?"
      ]
      if confirmation then pure res else cardView card
    Share _ -> do
      maybeCardValues <- div [Props._id "cardView"] [
        Nothing <$ cardActions card false
      , cardContent r.content
      ]
      case maybeCardValues of
        Nothing         -> cardView card
        Just secrets -> pure $ Share (Card r {secrets = secrets})
    _ -> pure res

cardActions :: Card -> Boolean -> Widget HTML CardEvent
cardActions card@(Card r) enabled = div [Props.className "cardActions"] [
    simpleButton   "exit"    "exit"     false        (Exit card)
  , simpleButton   "edit"    "edit"    (not enabled) (Edit card)
  , simpleButton   "clone"   "clone"   (not enabled) (Clone card)
  , if r.archived then
      simpleButton "restore" "restore" (not enabled) (Restore card)
    else
      simpleButton "archive" "archive" (not enabled) (Archive card)
  , simpleButton   "delete"  "delete"  (not enabled) (Delete card)
  -- , simpleButton   "share"   "share"   (not enabled) (Share card)         
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
, if (null ts) then (text "") else div [Props.className "card_tags"] [ul  []   $ (\s -> li' [text s]) <$> ts]
, if (null fs) then (text "") else div [Props.className "card_fields"] $ cardField <$> fs
, div [Props.className "card_notes"] [
    if (null ts && null fs) then (text "") else h3 [] [text "Notes"]
  , div [Props.className "markdown-body", Props.dangerouslySetInnerHTML { __html: unsafePerformEffect $ renderString n}] []
  ]
]

cardField :: forall a. CardField -> Widget HTML a
cardField f@(CardField {name, value, locked}) = do
  _ <- div [Props.className "fieldValue"] [
    div [Props.className "fieldLabel"] [text name]
  , dynamicWrapper (if locked then Just "PASSWORD" else Nothing) value $ textarea [Props.rows 1, Props.value value, Props.onClick, Props.disabled true] [] 
  , (if locked
    then (entropyMeter value)
    else (text "")
    )
  ] --TODO add class based on content for urls and emails
  _ <- pure $ copyToClipboard value
  cardField f
