module Functions.Card
  ( addTag
  , appendToTitle
  , archiveCard
  , createCardEntry
  , decodeDeltaCardObject
  , decryptCard
  , encodeDeltaCard
  , encryptCard
  , getFieldType
  , restoreCard
  )
  where

import Control.Alt ((<#>), (<$>))
import Control.Bind (bind, pure, (=<<))
import Control.Monad.Except (except, runExcept)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Core (Json, fromBoolean, fromObject, fromString, jsonEmptyObject, jsonSingletonObject, toBoolean, toObject, toString)
import Data.Array (elem, filter, fold, head, sort, tail)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Eq (eq)
import Data.Function ((#), ($))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer)
import Data.HeytingAlgebra (not)
import Data.Identifier (Identifier, computeIdentifier)
import Data.Lens (view)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (insert)
import Data.Set as Set
import Data.Show (class Show, show)
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.CardVersions.Card (class CardVersions, Card(..), CardField(..), CardValues(..), CardVersion(..), FieldType(..), _archived, _fields, _notes, _tags, _title, fromCard, toCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion, currentCardVersion)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..))
import DataModel.SRPVersions.SRP (HashFunction)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object, lookup, values)
import Foreign.Object as Object
import Functions.EncodeDecode (decryptJson, encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.Time (getCurrentTimestamp)

decryptCard :: ArrayBuffer -> CardReference -> ExceptT AppError Aff Card
decryptCard encryptedCard (CardReference {version, key}) =
  case version of 
    CardVersion_1    -> decryptCardJson currentCardCodecVersion

  where
    decryptCardJson :: forall a. CardVersions a => CA.JsonCodec a -> ExceptT AppError Aff Card
    decryptCardJson codec = toCard <$> ExceptT ((\cryptoKey -> decryptJson codec cryptoKey encryptedCard) =<< (importCryptoKeyAesGCM (toArrayBuffer key))) # mapError

    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

encryptCard :: Card -> HashFunction -> Aff (Tuple ArrayBuffer CardReference)
encryptCard card hashFunc = do
  identifier        :: Identifier  <- computeIdentifier
  cardKey           :: CryptoKey   <- generateCryptoKeyAesGCM
  encryptedCard     :: ArrayBuffer <- encryptJson currentCardCodecVersion cardKey (fromCard card)
  cardKeyHex        :: HexString   <- exportCryptoKeyToHex cardKey
  encryptedCardHash :: HexString   <- hashFunc (encryptedCard : Nil) <#> fromArrayBuffer
  pure $ Tuple encryptedCard (CardReference { reference: encryptedCardHash, identifier, key: cardKeyHex, version: currentCardVersion } )

-- ------------------------------------------------------------------------

encodeDeltaCard :: Card -> Json
encodeDeltaCard card = fromObject $
  Object.fromFoldable
    [ Tuple "label" $ fromString encodeTitle
    , Tuple "data"  $ fromObject $ Object.fromFoldable
        [ Tuple "directLogin"   jsonEmptyObject
        , Tuple "notes"       $ fromString (view _notes card)
        ]
    , Tuple "currentVersion" $ jsonSingletonObject "fields" $ fromObject $ Object.fromFoldable
        ((\(CardField {name, value, locked}) -> Tuple "00000000" $ fromObject $ Object.fromFoldable
          [ Tuple "label"  $ fromString name
          , Tuple "value"  $ fromString value
          , Tuple "actionType" $ if locked then fromString "PASSWORD" else fromString "NONE"
          , Tuple "hidden" $ fromBoolean locked
          ]
        ) <$> (view _fields card))
    ]

    where
      encodeTitle :: String
      encodeTitle = view _title card <> encodeTags <> (if (view _archived card) then " ARCH" else "")
      encodeTags :: String
      encodeTags = fold $ (\t -> " " <> t) <$> Set.toUnfoldable (view _tags card) # sort

decodeDeltaCardObject :: Object Json -> ExceptT AppError Aff Card
decodeDeltaCardObject obj = do
  timestamp <- liftEffect getCurrentTimestamp
  titleAndTags :: Array String <- split (Pattern " ") <$> (except $ note (ImportError "Cannot find card label") $ (toString =<< lookup "label" obj))
  let title    = fromMaybe "" $ head titleAndTags
  let tags     = filter (\s -> not $ eq "ARCH" s) $ fromMaybe [] $ tail titleAndTags
  let archived = elem "ARCH" titleAndTags
  fields :: Array CardField <- do
    a <- except $ note (ImportError "Cannot find card fields") $ (values <$> (toObject =<< (lookup "fields") =<< toObject =<< lookup "currentVersion" obj))
    except $ sequence (decodeCardField <$> a)
  notes  <- except $ note (ImportError "Cannot find card notes") $ (toString =<< (lookup "notes") =<< toObject =<< lookup "data" obj)
  pure $ Card { timestamp: timestamp
              , secrets: []
              , archived: archived
              , content: CardValues { title: title
                                    , tags: Set.fromFoldable tags
                                    , fields: fields
                                    , notes: notes
                                    }
              }
  where
    decodeCardField :: Json -> Either AppError CardField
    decodeCardField json = runExcept $ do
      obj'    <- except $ note (ImportError "Cannot convert json to json object") $ (toObject json)
      label  <- except $ note (ImportError "Cannot find field label")  $ (toString  =<< lookup "label"  obj')
      value  <- except $ note (ImportError "Cannot find field value")  $ (toString  =<< lookup "value"  obj')
      let hidden = fromMaybe false $ (toBoolean =<< lookup "hidden" obj')
      pure $ CardField {name: label, value: value, locked: hidden, settings: Nothing}

-- ------------------------------------------------------------------------

createCardEntry :: HashFunction -> Card -> Aff (Tuple ArrayBuffer CardEntry)
createCardEntry hashFunc card@(Card { content: (CardValues content), archived, timestamp }) = do
  Tuple encryptedCard cardReference <- encryptCard card hashFunc
  let cardEntry  = CardEntry { title: content.title
                             , tags: content.tags
                             , archived: archived
                             , lastUsed: timestamp
                             , cardReference
                             }
  pure $ Tuple encryptedCard cardEntry

appendToTitle :: String -> Card -> Card
appendToTitle titleAppend (Card card@{content: CardValues cardValues@{title}}) = Card (card {content = CardValues cardValues {title = title <> titleAppend} })

addTag :: String -> Card -> Card
addTag tag (Card card@{content: CardValues cardValues@{tags}}) = Card (card {content = CardValues cardValues {tags = insert tag tags}})

archiveCard :: Card -> Card
archiveCard (Card card) = Card card {archived = true}

restoreCard :: Card -> Card
restoreCard (Card card) = Card card {archived = false}
 
testRegex :: Either String Regex -> String -> Boolean
testRegex eitherRegex value =
  case eitherRegex of
    Left  _     -> false
    Right regex -> test regex value

emailRegex :: Either String Regex
emailRegex = regex "^([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+)\\.([a-zA-Z]{2,})$" noFlags

isValidEmail :: String -> Boolean
isValidEmail email = testRegex emailRegex email

urlRegex :: Either String Regex
urlRegex = regex "^https?://[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*(:[0-9]+)?(/.*)?$" noFlags

isValidUrl :: String -> Boolean
isValidUrl url = testRegex urlRegex url

getFieldType :: CardField -> FieldType
getFieldType (CardField { value, locked })
  | locked              = Passphrase
  | isValidEmail value  = Email
  | isValidUrl value    = Url
  | true                = None