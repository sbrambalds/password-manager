module Functions.User where

import Control.Alt ((<#>), (<$>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Function (flip, (#), ($))
import Data.HexString (HexString, fromArrayBuffer, splitHexInHalf, toArrayBuffer)
import Data.Lens (view)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..), fst)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRPVersions.CurrentSRPVersions (currentSRPVersion)
import DataModel.SRPVersions.SRP (HashFunction, SRPConf)
import DataModel.UserVersions.CurrentUserVersions (currentMasterKeyEncodingVersion, currentUserInfoCodecVersion)
import DataModel.UserVersions.User (class UserInfoVersions, MasterKey, MasterKeyEncodingVersion(..), RequestUserCard(..), UserInfo, UserInfoReferences, _userInfoRef_reference, _userInfo_identifier, toUserInfo)
import DataModel.UserVersions.UserCodecs (fromUserInfo, userInfoV1Codec, userInfoV2Codec, userInfoV3Codec)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.EncodeDecode (decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.SRP (prepareV)
import OperationalWidgets.Sync (SyncOperation(..))

computeRemoteUserCard :: SRPConf -> HexString -> HexString -> HexString -> HexString -> MasterKey -> ExceptT AppError Aff RequestUserCard
computeRemoteUserCard srpConf c p s originMasterKey masterKey = do
  v <- withExceptT (show >>> SRPError >>> ProtocolError) $ ExceptT (prepareV srpConf (toArrayBuffer s) (toArrayBuffer p))
  pure $ RequestUserCard { c, v, s, srpVersion: currentSRPVersion, originMasterKey: Just originMasterKey, masterKey }

computeMasterKey :: UserInfoReferences -> HexString -> Aff MasterKey
computeMasterKey {reference: userInfoHash, key: userInfoKey} p = do
  masterPassword <- importCryptoKeyAesGCM (toArrayBuffer p) # liftAff
  unencryptedMasterKeyContent <- concatArrayBuffers ((userInfoHash : userInfoKey : Nil) <#> toArrayBuffer) # liftEffect
  encryptedMasterKeyContent   <- encryptArrayBuffer masterPassword unencryptedMasterKeyContent
  pure $ Tuple (fromArrayBuffer encryptedMasterKeyContent) currentMasterKeyEncodingVersion

extractUserInfoReference :: MasterKey -> CryptoKey -> ExceptT AppError Aff UserInfoReferences
extractUserInfoReference (Tuple masterKeyContent masterKeyEncodingVersion) masterPassword = do
  case masterKeyEncodingVersion of
   MasterKeyEncodingVersion_1 -> decryptArrayBufferWithSplit
   MasterKeyEncodingVersion_2 -> decryptArrayBufferWithSplit
   MasterKeyEncodingVersion_3 -> decryptArrayBufferWithSplit

  where
    decryptArrayBufferWithSplit = do
      decryptedMasterKeyContent                      <- ExceptT $ decryptArrayBuffer masterPassword (toArrayBuffer masterKeyContent) <#> lmap (ProtocolError <<< CryptoError <<< show)
      let {before: userInfoHash, after: userInfoKey}  = splitHexInHalf (fromArrayBuffer decryptedMasterKeyContent)
      pure $ {reference: userInfoHash, key: userInfoKey}

decryptUserInfo :: ArrayBuffer -> HexString -> MasterKeyEncodingVersion -> ExceptT AppError Aff UserInfo
decryptUserInfo encryptedUserInfo key version =
  case version of 
    MasterKeyEncodingVersion_1 -> decryptJsonUserInfo userInfoV1Codec
    MasterKeyEncodingVersion_2 -> decryptJsonUserInfo userInfoV2Codec
    MasterKeyEncodingVersion_3 -> decryptJsonUserInfo userInfoV3Codec

  where
    decryptJsonUserInfo :: forall a. UserInfoVersions a => JsonCodec a -> ExceptT AppError Aff UserInfo
    decryptJsonUserInfo codec = (toUserInfo <$> ExceptT ((\cryptoKey -> decryptJson codec cryptoKey encryptedUserInfo) =<< (importCryptoKeyAesGCM (toArrayBuffer key)))) # mapError

    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

encryptUserInfo :: UserInfo -> HashFunction -> Aff (Tuple ArrayBuffer UserInfoReferences)
encryptUserInfo userInfo hashFunc = do
  userInfoKey           :: CryptoKey   <- generateCryptoKeyAesGCM
  encrytedUserInfo      :: ArrayBuffer <- encryptJson currentUserInfoCodecVersion userInfoKey (fromUserInfo userInfo)
  userInfoKeyHex        :: HexString   <- exportCryptoKeyToHex userInfoKey
  encryptedUserInfoHash :: HexString   <- hashFunc (encrytedUserInfo : Nil) <#> fromArrayBuffer
  pure $ Tuple encrytedUserInfo {reference: encryptedUserInfoHash, key: userInfoKeyHex}

type UpdateUserStateUpdateInfo = {userInfo :: UserInfo, masterKey :: MasterKey, userInfoReferences :: UserInfoReferences }

asMaybe :: UpdateUserStateUpdateInfo -> {userInfo :: Maybe UserInfo, masterKey :: Maybe MasterKey, userInfoReferences :: Maybe UserInfoReferences }
asMaybe {userInfo, masterKey, userInfoReferences } = {userInfo: Just userInfo, masterKey: Just masterKey, userInfoReferences: Just userInfoReferences}

-- ----------------------------------------------------------------------------------------

computeUserInfoSyncSteps :: AppState -> UserInfo -> ExceptT AppError Aff { updateUserInfoOp :: List (Tuple SyncOperation String), newMasterKey :: MasterKey, newUserInfoReference :: UserInfoReferences }
computeUserInfoSyncSteps {hash: hashFunc, srpConf, c: Just c, s: Just s, p: Just p, masterKey: Just masterKey, userInfo: Just userInfo, userInfoReferences: Just userInfoReferences} newUserInfo = do
  Tuple newEncryptedUserInfo newUserInfoReference <- encryptUserInfo  newUserInfo hashFunc      # liftAff
  newUser                                         <- computeRemoteUserCard srpConf c p s (fst masterKey) =<<
                                                    (computeMasterKey newUserInfoReference p    # liftAff)


  pure {
    updateUserInfoOp: ( ( Tuple (SaveBlob (_userInfoRef_reference  # flip view newUserInfoReference)
                                          (_userInfo_identifier # flip view newUserInfo)
                                          (newEncryptedUserInfo # fromArrayBuffer)
                                )
                                "Save User Info"
                        )
                      : ( Tuple (SaveUser    newUser)                                             
                                "Update User Info"
                        )
                      : ( Tuple (DeleteBlob (_userInfoRef_reference  # flip view userInfoReferences)
                                            (_userInfo_identifier # flip view userInfo)
                                )
                                "Delete old User Info"
                        )
                      :   Nil
                      )
  , newMasterKey: (unwrap newUser).masterKey
  , newUserInfoReference
  }
computeUserInfoSyncSteps _ _ = do
  throwError $ InvalidStateError (CorruptedState "computeUserInfoSyncSteps")