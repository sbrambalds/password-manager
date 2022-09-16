module Functions.Communication.Login where

import Affjax.RequestBody (RequestBody, json)
-- import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Control.Monad.State (StateT, modify_, get, mapStateT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromInt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, toBigInt, fromBigInt, toArrayBuffer, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState, AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (IndexReference)
import Effect.Aff (Aff)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk{- `, doGenericRequest'` -})
import SRP as SRP
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.State (makeStateT)
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

type LoginResult =  { indexReference :: IndexReference
                    , sessionKey     :: HexString
                    }

login :: SRP.SRPConf -> StateT AppState (ExceptT AppError Aff) LoginResult
login srpConf = do
  sessionKey :: HexString   <- makeStateT $ ExceptT $ (fromArrayBuffer >>> Right) <$> SRP.randomArrayBuffer 32
  modify_ (\currentState -> currentState { sessionKey = Just sessionKey })
  loginStep1Result <- loginStep1 srpConf
  { m1, kk, m2, encIndexReference: indexReference } <- loginStep2 srpConf loginStep1Result
  check :: Boolean <- makeStateT $ ExceptT $ Right <$> SRP.checkM2 SRP.baseConfiguration loginStep1Result.aa m1 kk (toArrayBuffer m2)
  case check of
    true  -> do
      modify_ (\currentState -> currentState { sessionKey = Just sessionKey })
      makeStateT $ except $ Right { indexReference, sessionKey }
    false -> makeStateT $ except $ Left (ProtocolError $ SRPError "Client M2 doesn't match with server M2")

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Response = { s  :: HexString
                          , bb :: HexString
                          }

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: SRP.SRPConf -> StateT AppState (ExceptT AppError Aff) LoginStep1Result
loginStep1 srpConf = do
  { proxy: _, c: mc, p: _, sessionKey: _, toll: _ } <- get
  c <- makeStateT $ except $ note (InvalidStateError "c is Nothing") mc
  (Tuple a aa) <- makeStateT $ withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" ["login", "step1", show c] :: String
  let body = json $ encodeJson { c, aa: fromBigInt aa }  :: RequestBody
  step1Response <- mapStateT (\e -> withExceptT(\err -> ProtocolError err) e) (manageGenericRequest url POST (Just body) RF.json)
  responseBody :: LoginStep1Response <- makeStateT $ except $ if isStatusCodeOk step1Response.status
                                                          then lmap (\err -> ProtocolError $ DecodeError $ show err) (decodeJson step1Response.body)
                                                          else Left (ProtocolError $ ResponseError (unwrap step1Response.status))
  bb :: BigInt <- makeStateT $ except $ note (ProtocolError $ SRPError "Error in converting B from String to BigInt") (toBigInt responseBody.bb)
  makeStateT $ except $ if bb == fromInt (0)
           then Left $ ProtocolError $ SRPError "Server returned B == 0"
           else Right { aa, a, s: responseBody.s, bb }

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LogintStep2Data = { aa :: BigInt
                       , bb :: BigInt
                       , a  :: BigInt
                       , s  :: HexString
                       }

type LoginStep2Response = { m2 :: HexString
                          , encIndexReference :: HexString
                          }

type LoginStep2Result = { m1 :: ArrayBuffer
                        , kk :: ArrayBuffer
                        , m2 :: HexString
                        , encIndexReference :: HexString
                        }

loginStep2 :: SRP.SRPConf -> LogintStep2Data -> StateT AppState (ExceptT AppError Aff) LoginStep2Result
loginStep2 srpConf { aa, bb, a, s } = do
  { proxy: _, c: mc, p: mp, sessionKey: _, toll: _ } <- get
  c <- makeStateT $ except $ note (InvalidStateError "c is Nothing") mc
  p <- makeStateT $ except $ note (InvalidStateError "p is Nothing") mp
  x  :: BigInt      <- makeStateT $ ExceptT $ (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab)) <$> (srpConf.kdf (toArrayBuffer s) (toArrayBuffer p))
  ss :: BigInt      <- makeStateT $ withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareSClient srpConf aa bb x a)
  kk :: ArrayBuffer <- makeStateT $ ExceptT $ Right <$> (SRP.prepareK srpConf ss)
  m1 :: ArrayBuffer <- makeStateT $ ExceptT $ Right <$> (SRP.prepareM1 srpConf c s aa bb kk)
  let url  = joinWith "/" ["login", "step2", show c] :: String
  let body = json $ encodeJson { m1: fromArrayBuffer m1 }  :: RequestBody
  step2Response <- mapStateT (\e -> withExceptT(\err -> ProtocolError err) e) (manageGenericRequest url POST (Just body) RF.json)
  responseBody :: LoginStep2Response <- makeStateT $ except $ if isStatusCodeOk step2Response.status
                                                              then lmap (\err -> ProtocolError $ DecodeError $ show err) (decodeJson step2Response.body)
                                                              else Left (ProtocolError $ ResponseError (unwrap step2Response.status))
  makeStateT $ except $ Right { m1, kk, m2: responseBody.m2, encIndexReference: responseBody.encIndexReference }