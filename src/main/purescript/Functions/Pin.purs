module Functions.Pin where

import Control.Alt ((<#>))
import Control.Alternative (pure, (*>))
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad.Except.Trans (ExceptT(..))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Either (note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.HexString (Base(..), hex, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodeUnits (length)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRPVersions.SRP (HashFunction)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.OneTimeShare (PIN)
import Functions.EncodeDecode (decryptCredentials, encryptCredentials, importCryptoKeyAesGCM)
import Views.LoginFormView (Username, Password)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

makeKey :: String -> String
makeKey = (<>) "clipperz.epsilon.pin."

pinUsernameKey :: String
pinUsernameKey = makeKey "username"

pinPassphraseKey :: String
pinPassphraseKey = makeKey "passphrase"

pinFailureCountKey :: String
pinFailureCountKey = makeKey "failureCount"


isPinValid :: PIN -> Boolean
isPinValid p = (length p) == 5

generateKeyFromPin :: HashFunction -> String -> Aff CryptoKey
generateKeyFromPin hashf pin = do
  pinBuffer <- hashf $ (toArrayBuffer $ hex pin) : Nil
  importCryptoKeyAesGCM pinBuffer

decryptPassphraseWithPin :: HashFunction -> PIN -> ExceptT AppError Aff Credentials
decryptPassphraseWithPin hashFunc pin = do  
  storage              <- liftEffect $ window >>= localStorage
  username             <- getItem pinUsernameKey   storage <#> note (InvalidStateError (CorruptedSavedPassphrase "user not found in local storage"))       # liftEffect # ExceptT
  pinEncryptedPassword <- getItem pinPassphraseKey storage <#> note (InvalidStateError (CorruptedSavedPassphrase "passphrase not found in local storage")) # liftEffect # ExceptT
  
  password <- decryptCredentials (hex pinEncryptedPassword) =<< (generateKeyFromPin hashFunc pin # liftAff)

  pure $ { username, password }

deleteCredentials :: Effect Unit
deleteCredentials = do
  storage <- liftEffect $ window >>= localStorage
  removeItem pinUsernameKey     storage
  removeItem pinPassphraseKey   storage
  removeItem pinFailureCountKey storage

encryptedPassphraseByteLength :: Int
encryptedPassphraseByteLength = 1024

savePinEncryptedCredentials :: Username -> Password -> HashFunction -> String -> ExceptT AppError Aff Unit
savePinEncryptedCredentials username password hashf pin = do
  pinEncryptedPassword <- encryptCredentials password =<< (generateKeyFromPin hashf pin # liftAff)

  storage <- liftEffect $ window >>= localStorage
  liftEffect $ setItem pinUsernameKey      username                           storage
  liftEffect $ setItem pinPassphraseKey   (toString Hex pinEncryptedPassword) storage
  liftEffect $ setItem pinFailureCountKey (show 0)                            storage

  pure unit

pinExists :: Effect Boolean
pinExists = do
  storage <- localStorage =<< window
  maybePassphrase <- (getItem pinPassphraseKey storage)
  maybeUsername   <- (getItem pinUsernameKey       storage)
  pure $ isJust (maybePassphrase *> maybeUsername)
