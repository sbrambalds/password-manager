module Functions.Passkey
  ( checkPasskeyCreationResponse
  , deletePasskey
  , getCreationOptions
  , getCredentialsWithPasskey
  , getPRFKey
  , getRequestOptions
  , passkeyExists
  , passkeyIdKey
  , passkeyUsernameKey
  , registerPasskey
  , saveCredentialsWithPasskey
  )
  where

import Control.Alt ((<#>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.Eq ((==))
import Data.Function ((#), ($), (>>>))
import Data.HexString (Base(..), HexString, fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.HeytingAlgebra ((&&))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid ((<>))
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRPVersions.SRP (HashFunction)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.ArrayBuffer (base64Decoding)
import Functions.EncodeDecode (decryptCredentials, encryptCredentials, importCryptoKeyAesGCM)
import Functions.SRP (randomArrayBuffer)
import Views.LoginFormView (Password, Username)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Webauthn.PublicKeyCredential (AuthenticatorAttestationResponse, Extensions(..), PublicKeyAlgorithm(..), PublicKeyCredentialCreationOptions, PublicKeyCredentialRequestOptions, create, defaultCreationOptions, get, getClientData, getPRFResult, isPRFEnabled)

type C = HexString
type Id = ArrayBuffer

makeKey :: String -> String
makeKey = (<>) "clipperz.epsilon.passkey."

passkeyIdKey :: String
passkeyIdKey = makeKey "id"

passkeyUsernameKey :: String
passkeyUsernameKey = makeKey "username"

passkeyPassphraseKey :: String
passkeyPassphraseKey = makeKey "passphrase"

passkeyExists :: Effect Boolean
passkeyExists = do
  storage <- localStorage =<< window
  maybePassphrase <- (getItem passkeyPassphraseKey storage)
  maybeUsername   <- (getItem passkeyUsernameKey   storage)
  maybeId         <- (getItem passkeyIdKey         storage)
  pure $ isJust (maybePassphrase *> maybeUsername *> maybeId)

deletePasskey :: Effect Unit
deletePasskey = do
  storage <- window >>= localStorage
  removeItem passkeyIdKey         storage
  removeItem passkeyUsernameKey   storage
  removeItem passkeyPassphraseKey storage

saveCredentialsWithPasskey :: Username -> Password -> Id -> CryptoKey -> ExceptT AppError Aff Unit
saveCredentialsWithPasskey username password id key = do
  passkeyEncryptedPassword <- encryptCredentials password key

  storage <- liftEffect $ window >>= localStorage
  setItem passkeyUsernameKey     username                               storage # liftEffect
  setItem passkeyPassphraseKey  (toString Hex passkeyEncryptedPassword) storage # liftEffect
  setItem passkeyIdKey          (toString Hex $ fromArrayBuffer id)     storage # liftEffect

  pure unit

getCredentialsWithPasskey :: CryptoKey -> ExceptT AppError Aff Credentials
getCredentialsWithPasskey key = do
  storage                  <- liftEffect $ window >>= localStorage
  username                 <- getItem passkeyUsernameKey   storage <#> note (InvalidStateError (CorruptedSavedPassphrase "user not found in local storage"))       # liftEffect # ExceptT
  passkeyEncryptedPassword <- getItem passkeyPassphraseKey storage <#> note (InvalidStateError (CorruptedSavedPassphrase "passphrase not found in local storage")) # liftEffect # ExceptT
  
  password <- decryptCredentials (hex passkeyEncryptedPassword) key

  pure $ { username, password }

-- ==========================================

registerPasskey :: C -> Username -> ExceptT AppError Aff Id
registerPasskey c username = do
  creationOptions <- getCreationOptions c username # liftAff
  createResponse  <- create creationOptions        # ExceptT # withExceptT (\_ -> OperationCanceled)
  case isPRFEnabled createResponse of
    false -> throwError $ OperationUnsupported
    true  -> log "PRF enabled" # liftEffect
  origin <- window >>= location >>= origin # liftEffect
  case checkPasskeyCreationResponse origin creationOptions.challenge createResponse.response of
    false -> throwError $ UnhandledCondition "Challenge not OK"
    true  -> pure $ createResponse.rawId

getPRFKey :: HashFunction -> Username -> Id -> ExceptT AppError Aff CryptoKey
getPRFKey hashFunc username id =
      (getRequestOptions hashFunc username id # liftAff)
  >>= (get >>> ExceptT >>> withExceptT (\_ -> OperationCanceled))
  >>= (getPRFResult >>> importCryptoKeyAesGCM >>> liftAff)

getRequestOptions :: HashFunction -> Username -> Id -> Aff PublicKeyCredentialRequestOptions
getRequestOptions hashFunc username id = do
  challenge <- randomArrayBuffer 32
  salt      <- hashFunc $ (toArrayBuffer $ hex username) : Nil
  pure  { challenge
        , timeout: Nothing
        , rpId: Nothing
        , allowCredentials: Just [ {id, transports: Nothing }]
        , userVerification: Nothing
        , extensions: [ PRF salt ]
        }

getCreationOptions :: C -> Username -> Aff PublicKeyCredentialCreationOptions
getCreationOptions c username = do
  challenge <- randomArrayBuffer 32
  pure $ defaultCreationOptions { 
          rp: { icon: Just "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADkAAAA5CAYAAACMGIOFAAAAAXNSR0IArs4c6QAABuxJREFUaN7tWl1oHFUUTvwvUn2oiOBLxWDFtDszu9smeRGf+iIKiikIPhTFFn0RX7SI4GqS3dn8bExSfwqWYhHEPAgV8Q+k/tOWJJ17N2nTJG1jQ0BEihLQ7GZ3rufcn93JZjczsz/dJOyFyyQ7O/ee75xzz/nOmW1qaozGaAwckSZ2k9e5CeGx5q1tvcdO34LXmH5+Z0wjX8d0+i1eowb5pujUre/gO19GWs/dp6y/8UG2Tt2G175gMpAIzrF3Q1dZIjTHBkOXi0683x+8xLp2jz2Iz3V2jt684UEeC43dildTs1pjOknDtGO6tQIzs3aSFXGf/IuW5yCbNhHIngDZjQBMPclMAYQVm3gfldEVIA9sUpA0K0E0QDZANkA2QPpP8GJzdyZzY0CyZsyno52sOgphBcAQ8CEAUoqZ1Aok7sf3lYyqlHxl808zSJ7oAQZTjN0IUPnvVgzSwXgQAK6nWJRzwPrBHv384xUBVRqL7ru4A4RI9RoXUJgfozp9zdxD9xQDjFZQApULcpS7YX4d5+jVp3QgFm8AW/q1PzjNojpZSnQsbBOWjvjnvDlhNevAQHCGCzQYusKGw79Loek5uL4V06aM1cphXDmmMaH5ARkLjrcUswrQvb0xg74DZH4c1uP7oxzIohCosmYxpbgfbHk2QICPhwSwZbQoCmQaSU66h0LzSkhiGrSr25gIq814FSJpnReQ/aGxe/A5PHfRAGkzNRqDzyfjxiSQ+HmGZD+nEC6HlcbPo4b1ofOY+Cp28Rrf9ct2WHABqwRBpqkSLivJNU6Glh4OX2PCpa1LIOCbAPIg3ncFaSSFAg3yLCoKnp/pMy5yiykPcuyVdTybQbnAZeeUYkf9VDAqNMc0a38iOKtArWMNkpFWtvH7I+EFhoKu90zBtPuMaf7cgNjPlhbLln6G8nu4DyjnUd/WzBW+Bh0Zho1hwWWHFUttKq9YVnHArIyZks8z9/34TKMHgZymU25fQKGivyxcldd+HgXNCWf7BGj7V4qVTkDhjTFBpTFP6URpozsw3o6HPq/ZDTmz0m3tnuB4wHNnIR8dyXsftf3Dg4kprKMOfwb+t+sBSuyrOgoWygHyTbHj7UsYF3qcKcwTjYsbyTAc6AFYMIkREnsx2JMR1iXy3Ck3pjUCljvnGREXyAqCUr0hec+Ca69pTGq+2M+ahAwJXzAN8jOGe8yPQ+F5mTK4VtMq2lURYFYGsBWMoJirMS+KfhD5ASL/64VU0ze9K0WtkJlENeswbHQK5t+oURRAuDWxq+WWeWBX8LPrAPpzYD0v9En6t4ZSVtLlW48kRwLkXkj6nRCFT4Igf2CuqxSoAgh/L4KHnACLPRWRbMitOKhe2x+iLx7uQtfA/6EN+VwF59OWAS4LnvJ0YSmHBBz35fuXQ8bLHM3KysMts7eLs0s+QE7rL6+uynsZmfd6cb1Ex2/bEJRUaP1ePeRSjmY9fzS8qAKGXZ6riueQxcQN8kyZ1UVN33v8l08vFaWUFRm1r+OZL4uu1eLNFVQQP8m8lTKrkzOXeXlnkC8cVVFz3dwUyypOknnVQF3dUVYstgfX5eQb0sZLdXFbtWFcm9oXBwbikWDz+6L4pZ6+L2pNKwPR9qGyCuNK0og6J6DtaVlvpt0ElucV2iHkFbj+6V7dcEWkB0W0PVO4f42DjSDAUJG/P8JrTd4W8Vb3QU9IRWJxhr2kGku4rUbfviFuqzaAc/IkalieLzeCvTqI5FPOcQncLVjlOgBRg3bU1G3zLcqz0KKkf0nq5WaJtOzTLMR3TW93CojcGNa5IMjDekGLOtahs4fk81V3W+eCSMyHRDXg1uLIqnqvO0DanVYseFEry7fSristnRKWt475blr5aWyZhnVEVgWeziG6KQaaYmfJkYIOC6V56kCkRKOLHvRcIHtP+iLxw+KfHN276Gg60XW0jgDpZ04lFa6bU55GPpW93fXOZwotjtaEQmAg72FVqkKcFQds9jKmBBnaU0WbS5ALsR+qiHuplziqBozsPH0HPHtVWMkqpIXY8kjLI7JkaskDDsZVXRaEQBXYqDHxCGw4IVIIVY1mHohEAqcMXxN4CfnqfOLrADy/qtxytDcZb4sa5Ptuzbq/Juex0MWUdWQa6Meo1wfvJNQrBN661+mLeN/5XW+VDH1VphXs8y6jZZFRwauAI+q7Xtes2ttkIVhyP3YF0H2FZa0TfisHXhDLCA5ufgqBygA3gxZWCq5ioPHXFsG/oe9yJ/6EDABOqkDgN48ppUTaztwFrnkN5sncWtzSdfy93nDLVzn3MUNjd1eSqFUgij58dkehK9d9ON2oUiaigDLunnUrlku7b7WoFq7DtvrPSRujMRpja4//AaDTfP1l7iGQAAAAAElFTkSuQmCC"
              , name: "Clipperz"
              , id:   Nothing
              }
        , user: { icon: Nothing
                , id:   toArrayBuffer c
                , name: username
                , displayName: username
                }
        , challenge
        , pubKeyCredParams: [ ECDSA_WITH_SHA256, RSA_WITH_SHA256 ]
        , extensions: [ PRF (toArrayBuffer $ hex "registration") ]
        }

checkPasskeyCreationResponse :: String -> ArrayBuffer -> AuthenticatorAttestationResponse -> Boolean
checkPasskeyCreationResponse origin originalChallenge response = (do
    clientData <- getClientData response # lmap (\_ -> unit)
    challenge  <- base64Decoding clientData.challenge <#> fromArrayBuffer # lmap (\_ -> unit)
    Right $  fromArrayBuffer originalChallenge == challenge 
          && clientData.type == "webauthn.create"
          && clientData.origin == origin
  ) # hush # case _ of
    Nothing -> false
    Just res -> res