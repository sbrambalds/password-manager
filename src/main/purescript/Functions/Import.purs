module Functions.Import where

import Control.Bind (bind, pure, (<#>), (<$>), (=<<))
import Control.Monad.Except (ExceptT, except, throwError, withExceptT)
import Data.Argonaut.Core (caseJsonArray, caseJsonObject, stringify)
import Data.Argonaut.Decode (parseJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (decode, encode)
import Data.Either (hush, note)
import Data.Function ((#), ($), (<<<), (>>>))
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence)
import DataModel.AppError (AppError(..))
import DataModel.CardVersions.Card (Card, CardVersion(..), cardVersionCodec, toCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Functions.Card (decodeDeltaCardObject)
import Web.File.Blob (Blob)
import Web.File.File (File)

foreign import _readFile :: File -> EffectFnAff String

readFile :: Maybe File -> ExceptT AppError Aff String
readFile maybeFile = do
  file   <- except $ note (ImportError "File not found") maybeFile
  liftAff $ fromEffectFnAff (_readFile file)

foreign import decodeHTML :: String -> String

foreign import createFile :: Blob -> File

data ImportVersion = Delta | Epsilon CardVersion

instance showImportVersion :: Show ImportVersion where
  show  Delta      = "Delta"
  show (Epsilon v) = "Epsilon: " <> (stringify $ encode cardVersionCodec v)

parseImport :: String -> ExceptT AppError Aff (Array Card)
parseImport html = do
  regex <- except $ regex "<textarea( class=\'({\"tag\":\".+\"})\')?>(\\[[\\s\\S]+\\])<\\/textarea>" noFlags # lmap (\_ -> ImportError "The regex written by the developers is not correct")
  case (match regex (decodeHTML html) <#> fromFoldable) of
    Just (_ : _ : maybeVersion : (Just cards) : Nil) -> do
      version <- pure $ fromMaybe Delta (Epsilon <$> (
                                               (decode cardVersionCodec >>> hush)
                                           =<< (parseJson               >>> hush)
                                           =<<  maybeVersion
                                        ))                             
      decodeImport version cards
    _                                                -> 
      throwError $ ImportError ("Invalid file: unable to decode data [" <> decodeHTML html <> "]")

decodeImport :: ImportVersion -> String -> ExceptT AppError Aff (Array Card)
decodeImport version cards = 
  (\json -> caseJsonArray (throwError $ ImportError "Cannot convert json to json array") (\array -> sequence $ array <#> 
    (\card -> case version of
      Delta                 -> caseJsonObject (throwError $ ImportError "Cannot conver json to json object") decodeDeltaCardObject card
      Epsilon CardVersion_1 -> (except $ toCard <$> decode currentCardCodecVersion card) # withExceptT (ProtocolError <<< DecodeError <<< show)
    )
  ) json) =<< (except $ lmap (ProtocolError <<< DecodeError <<< show) (jsonParser cards))