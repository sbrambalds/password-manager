module Functions.Index where

import Control.Alt ((<#>), (<$>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except (throwError)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (JsonCodec)
import Data.Function (flip, (#), ($))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer)
import Data.Identifier (computeIdentifier)
import Data.Lens (set, view)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.IndexVersions.CurrentIndexVersions (currentIndexCodecVersion, currentIndexVersion)
import DataModel.IndexVersions.Index (class IndexVersions, Index, IndexVersion(..), _index_identifier, fromIndex, toIndex)
import DataModel.IndexVersions.IndexV1 (indexV1Codec)
import DataModel.SRPVersions.SRP (HashFunction)
import DataModel.UserVersions.User (IndexReference(..), MasterKey, UserInfo, UserInfoReferences, _indexReference, _index_reference, _userInfo_identifier)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (decryptJson, encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.User (computeUserInfoSyncSteps)
import OperationalWidgets.Sync (SyncOperation(..))

decryptIndex :: IndexReference -> ArrayBuffer -> ExceptT AppError Aff Index
decryptIndex (IndexReference {version, key}) encryptedIndex =
  case version of 
    IndexVersion_1 -> decryptJsonIndex indexV1Codec

  where
    decryptJsonIndex :: forall a. IndexVersions a => JsonCodec a -> ExceptT AppError Aff Index
    decryptJsonIndex codec = toIndex <$> ExceptT ((\cryptoKey -> decryptJson codec cryptoKey encryptedIndex) =<< (importCryptoKeyAesGCM (toArrayBuffer key))) # mapError

    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

encryptIndex :: Index -> HashFunction -> Aff (Tuple ArrayBuffer IndexReference)
encryptIndex index hashFunc = do
  indexKey           :: CryptoKey   <- generateCryptoKeyAesGCM
  encryptedIndex     :: ArrayBuffer <- encryptJson currentIndexCodecVersion indexKey (fromIndex index)
  indexKeyHex        :: HexString   <- exportCryptoKeyToHex indexKey
  encryptedIndexHash :: HexString   <- hashFunc (encryptedIndex : Nil) <#> fromArrayBuffer
  pure $ Tuple encryptedIndex (IndexReference { reference: encryptedIndexHash, key: indexKeyHex, version: currentIndexVersion } )

-- ----------------------------------------------------------------------------------------

computeIndexSyncSteps :: AppState -> Index -> ExceptT AppError Aff { updateIndexOp :: List (Tuple SyncOperation String), newMasterKey :: MasterKey, newUserInfo :: UserInfo, newUserInfoReference :: UserInfoReferences }
computeIndexSyncSteps state@{hash: hashFunc, index: Just index, userInfo: Just userInfo} newIndex = do
  Tuple newEncryptedIndex newIndexReference <- encryptIndex newIndex hashFunc # liftAff
  newUserInfo                               <- (\id ->  ( set _userInfo_identifier id >>>
                                                          set _indexReference newIndexReference
                                                        ) userInfo
                                                ) <$> (computeIdentifier      # liftAff)
  { updateUserInfoOp
  , newMasterKey, newUserInfoReference }    <- computeUserInfoSyncSteps state newUserInfo

  pure {
    updateIndexOp:  ( ( Tuple (SaveBlob (_index_reference  # flip view newUserInfo)
                                        (_index_identifier # flip view newIndex)
                                        (newEncryptedIndex # fromArrayBuffer)
                              )
                              "Save Index"
                      )
                    : Nil
                    ) 
                    <>
                    updateUserInfoOp 
                    <>
                    ( ( Tuple (DeleteBlob (_index_reference  # flip view userInfo)
                                          (_index_identifier # flip view index)
                              )
                              "Delete old Index"
                      )
                    :   Nil
                    ) 
  , newUserInfo
  , newUserInfoReference
  , newMasterKey
  }
computeIndexSyncSteps _ _ = do
  throwError $ InvalidStateError (CorruptedState "computeIndexSyncSteps")