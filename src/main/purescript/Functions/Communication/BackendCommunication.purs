module Functions.Communication.BackendCommunication where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, withExceptT)
import Control.Monad.State (StateT, get, modify_, modify)
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (hex)
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import DataModel.AppState as AS
import DataModel.Proxy (Proxy(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.State (makeStateT)
import Functions.HashCash (TollChallenge, computeReceipt)
import Functions.JSState (getAppState, updateAppState)
import Functions.SRP (hashFuncSHA256)

import Effect.Class.Console (log)

-- ----------------------------------------------------------------------------

type Url = String

-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-usersession-id"

tollHeaderName :: String
tollHeaderName = "clipperz-hashcash-tollchallenge"

tollCostHeaderName :: String
tollCostHeaderName = "clipperz-hashcash-tollcost"

tollReceiptHeaderName :: String
tollReceiptHeaderName = "clipperz-hashcash-tollreceipt"

createHeaders :: AS.AppState -> Array RequestHeader
createHeaders { toll, sessionKey } = 
  let
    tollHeader    = (\t ->   RequestHeader tollReceiptHeaderName (show t))   <$> fromMaybe toll
    sessionHeader = (\key -> RequestHeader sessionKeyHeaderName  (show key)) <$> fromMaybe sessionKey
  in tollHeader <> sessionHeader

-- ----------------------------------------------------------------------------

manageGenericRequest :: forall a. Url -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> StateT AS.AppState (ExceptT ProtocolError Aff) (AXW.Response a)
manageGenericRequest url method body responseFormat = do
  currentState@{ c: _, p: _, toll: _, sessionKey: _, proxy } <- get
  let requestInfo = case proxy of
                      OnlineProxy _ -> OnlineRequestInfo  { url 
                                                          , method
                                                          , headers: createHeaders currentState
                                                          , body
                                                          , responseFormat
                                                          }
                      OfflineProxy  -> OfflineRequestInfo { url, method, body, responseFormat }
  modify_ (\state -> state { toll = Nothing })
  response <- makeStateT $ ExceptT $ doGenericRequest proxy requestInfo
  manageResponse response.status response

  where 
        manageResponse :: StatusCode -> (AXW.Response a -> StateT AS.AppState (ExceptT ProtocolError Aff) (AXW.Response a))
        manageResponse code@(StatusCode n)
          | n == 400            = \response -> do
              -- _ <- log "400 received"
              -- _ <- log $ show response.headers
              case (extractChallenge response.headers) of
                Just challenge -> do
                  -- _ <- log $ "toll challenge: " <> (show challenge)
                  receipt <- makeStateT $ ExceptT $ Right <$> computeReceipt hashFuncSHA256 challenge --TODO change hash function with the one in state
                  modify_ (\currentState -> currentState { toll = Just receipt })
                  manageGenericRequest url method body responseFormat
                Nothing -> makeStateT $ except $ Left $ IllegalResponse "HashCash headers not present or wrong"
          | isStatusCodeOk code = \response -> do
              -- _ <- log "200 received"
              case (extractChallenge response.headers) of
                Just challenge -> do
                  _ <- log "1 - computing new receipt..."
                  receipt <- makeStateT $ ExceptT $ Right <$> computeReceipt hashFuncSHA256 challenge --TODO change hash function with the one in state
                  _ <- log "2 - computed new receipt"
                  _ <- modify (\currentState -> currentState { toll = Just receipt })
                  _ <- log "3 - inserted new receipt into state"
                  pure response
                Nothing -> pure response
          | otherwise           = \_ -> do
            -- _ <- log $ "Unknown request error" <> show response.status
            makeStateT $ except $ Left $ ResponseError n
        
        extractChallenge :: Array ResponseHeader -> Maybe TollChallenge
        extractChallenge headers =
          let tollArray = filter (\a -> name a == tollHeaderName) headers
              costArray = filter (\a -> name a == tollCostHeaderName) headers
          in case (Tuple tollArray costArray) of
              Tuple [tollHeader] [costHeader] -> (\cost -> { toll: hex $ value tollHeader, cost }) <$> fromString (value costHeader)
              _                               -> Nothing

manageGenericRequest' :: forall a. Url -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AS.AppError Aff (AXW.Response a)
manageGenericRequest' url method body responseFormat = do
  currentState@{ proxy: proxy, c: mc, p: _, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  let requestInfo = case proxy of
                      OnlineProxy _ -> OnlineRequestInfo  { url 
                                                          , method
                                                          , headers: createHeaders currentState
                                                          , body
                                                          , responseFormat
                                                          }
                      OfflineProxy  -> OfflineRequestInfo { url, method, body, responseFormat }
  ExceptT $ Right <$> updateAppState (currentState { toll = Nothing })
  response <- withExceptT (\e -> AS.ProtocolError e) (ExceptT $ doGenericRequest proxy requestInfo)
  manageResponse response.status response

  where 
        manageResponse :: StatusCode -> (AXW.Response a -> ExceptT AS.AppError Aff (AXW.Response a))
        manageResponse code@(StatusCode n)
          | n == 400            = \response -> do
              -- _ <- log "400 received"
              -- _ <- log $ show response.headers
              case (extractChallenge response.headers) of
                Just challenge -> do
                  -- _ <- log $ "toll challenge: " <> (show challenge)
                  receipt <- ExceptT $ Right <$> computeReceipt hashFuncSHA256 challenge --TODO change hash function with the one in state
                  currentState <- ExceptT $ liftEffect $ getAppState
                  ExceptT $ Right <$> updateAppState (currentState { toll = Just receipt })
                  manageGenericRequest' url method body responseFormat
                Nothing -> except $ Left $  AS.ProtocolError $ IllegalResponse "HashCash headers not present or wrong"
          | isStatusCodeOk code = \response -> do
              -- _ <- log "200 received"
              case (extractChallenge response.headers) of
                Just challenge -> do
                  _ <- log "1 - computing new receipt..."
                  receipt <- ExceptT $ Right <$> computeReceipt hashFuncSHA256 challenge --TODO change hash function with the one in state
                  _ <- log "2 - computed new receipt"
                  currentState <- ExceptT $ liftEffect $ getAppState
                  ExceptT $ Right <$> updateAppState (currentState { toll = Just receipt })
                  _ <- log "3 - inserted new receipt into state"
                  pure response
                Nothing -> pure response
          | otherwise           = \_ -> do
            -- _ <- log $ "Unknown request error" <> show response.status
           except $ Left $ AS.ProtocolError $ ResponseError n
        
        extractChallenge :: Array ResponseHeader -> Maybe TollChallenge
        extractChallenge headers =
          let tollArray = filter (\a -> name a == tollHeaderName) headers
              costArray = filter (\a -> name a == tollCostHeaderName) headers
          in case (Tuple tollArray costArray) of
              Tuple [tollHeader] [costHeader] -> (\cost -> { toll: hex $ value tollHeader, cost }) <$> fromString (value costHeader)
              _                               -> Nothing

doGenericRequest :: forall a. Proxy -> RequestInfo a -> Aff (Either ProtocolError (AXW.Response a))
doGenericRequest (OnlineProxy baseUrl) (OnlineRequestInfo { url, method, headers, body, responseFormat }) =
  lmap (\e -> RequestError e) <$> AXW.request (
    AXW.defaultRequest {
        url            = joinWith "/" [baseUrl, url]
      , method         = Left method
      , headers        = headers
      , content        = body 
      , responseFormat = responseFormat
    }
  )
doGenericRequest  OfflineProxy   (OfflineRequestInfo { url: _, method: _, body: _, responseFormat: _ }) =
  pure $ Left $ ResponseError 500 -- TODO
doGenericRequest (OnlineProxy _) (OfflineRequestInfo _) = pure $ Left $ IllegalRequest "Cannot do an offline request with an online proxy"
doGenericRequest  OfflineProxy   (OnlineRequestInfo  _) = pure $ Left $ IllegalRequest "Cannot do an online request with an offline proxy"

data RequestInfo a = OnlineRequestInfo  { url :: Url 
                                        , method :: Method
                                        , headers :: Array RequestHeader
                                        , body :: Maybe RequestBody
                                        , responseFormat :: RF.ResponseFormat a
                                        } 
                   | OfflineRequestInfo { url :: Url 
                                        , method :: Method
                                        , body :: Maybe RequestBody
                                        , responseFormat :: RF.ResponseFormat a
                                        }

isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))
