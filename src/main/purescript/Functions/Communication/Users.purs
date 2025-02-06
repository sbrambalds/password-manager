module Functions.Communication.Users where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Data.Codec.Argonaut (encode)
import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.Common (joinWith)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Proxy (ProxyResponse(..))
import DataModel.UserVersions.User (RequestUserCard, UserCard, requestUserCardCodec, userCardCodec)
import Effect.Aff (Aff)
import Functions.Communication.Backend (genericRequest, isStatusCodeOk)

updateUserCard :: ConnectionState -> HexString -> UserCard -> ExceptT AppError Aff (ProxyResponse Unit)
updateUserCard connectionState c newUserCard = do
  let url = joinWith "/" ["users", toString Hex c]
  let body = (json $ encode userCardCodec newUserCard) :: RequestBody
  ProxyResponse proxy' response <- genericRequest connectionState url PATCH (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy' unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

deleteUserCard :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse Unit)
deleteUserCard connectionState c = do
  let url = joinWith "/" ["users", toString Hex c]
  ProxyResponse proxy response <- genericRequest connectionState url DELETE Nothing RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

changeUserPassword :: ConnectionState -> HexString -> RequestUserCard -> ExceptT AppError Aff (ProxyResponse Unit)
changeUserPassword connectionState oldC userCard = do
  let url         = joinWith "/" [ "users", toString Hex oldC ]
  let body        = (json $ encode requestUserCardCodec userCard) :: RequestBody
  ProxyResponse proxy' response <- genericRequest connectionState url PUT (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure       $ ProxyResponse proxy' unit
    else throwError $ ProtocolError (ResponseError (unwrap response.status))
