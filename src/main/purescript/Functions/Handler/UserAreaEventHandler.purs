module Functions.Handler.UserAreaEventHandler
  ( handleUserAreaEvent
  )
  where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Control.Alt (map, ($>), (<#>), (<$>))
import Control.Alternative ((*>), (<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category (identity, (<<<))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Argonaut.Core (stringify)
import Data.Array (filter, foldM, length)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), isRight)
import Data.Eq ((==))
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Function (flip, (#), ($), (>>>))
import Data.FunctorWithIndex (mapWithIndex)
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, hex, toArrayBuffer)
import Data.HeytingAlgebra (not)
import Data.Identifier (computeIdentifier)
import Data.Lens (set, view)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (insert)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (unit)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState, CardsCache)
import DataModel.CardVersions.Card (Card, CardVersion(..), fromCard)
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import DataModel.Communication.ConnectionState (ConnectionState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), _card_identifier, _card_reference, addToIndex)
import DataModel.Proxy (DataOnLocalStorage(..), DynamicProxy(..), Proxy(..), ProxyInfo, ProxyResponse(..), defaultOnlineProxy)
import DataModel.SRPVersions.SRP (hashFuncSHA256)
import DataModel.UserVersions.CurrentUserVersions (currentMasterKeyEncodingVersion)
import DataModel.UserVersions.User (IndexReference(..), UserInfo(..), _userInfo_identifier, _userPreferences)
import DataModel.WidgetState (CardManagerState, CardViewState(..), ImportStep(..), LoginType(..), MainPageWidgetState, Page(..), PagesState, UserAreaPage(..), UserAreaState, WidgetState(..), _mainPagesState)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Card (addTag, createCardEntry)
import Functions.Communication.Backend (genericRequest)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Cards (getCard)
import Functions.Communication.SyncBackend (syncBackend)
import Functions.DeviceSync (computeDeleteOperations, computeSyncOperations, updateSyncPreference)
import Functions.EncodeDecode (decryptArrayBuffer, encryptArrayBuffer, importCryptoKeyAesGCM)
import Functions.Events (eventDelayed, focus)
import Functions.Export (BlobsList, appendCardsDataInPlace, getBasicHTML, prepareHTMLBlob, prepareUnencryptedExport)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, defaultPagesState, handleOperationResult, noOperation, pagesInfoWithLogin, pagesInfoWithMain, runStep, runWidgetStep, syncLocalStorage)
import Functions.Import (ImportVersion(..), decodeImport, parseImport, readFile)
import Functions.Index (computeIndexSyncSteps)
import Functions.Pin (deleteCredentials, pinExists, pinUsernameKey, saveCredentials)
import Functions.SRP as SRP
import Functions.State (resetState)
import Functions.Time (formatDateTimeToDate, getCurrentDateTime)
import Functions.Timer (activateTimer, stopTimer)
import Functions.User (computeRemoteUserCard, computeUserInfoSyncSteps)
import OperationalWidgets.Sync (SyncOperation(..), addPendingOperation)
import Views.DonationViews as DonationEvent
import Views.ExportView (ExportEvent(..))
import Views.LoginFormView (Username, emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SetPinView (PinEvent(..))
import Views.UserAreaView (UserAreaEvent(..), userAreaInitialState)
import Web.DownloadJs (download)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState


handleUserAreaEvent userAreaEvent cardManagerState userAreaState state@{proxy, srpConf, hash: hashFunc, cardsCache, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {indexReference: IndexReference { reference: indexRef}, userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, pinExists, enableSync, donationLevel: Just donationLevel, syncDataWire} proxyInfo f = do
  let defaultMainState =  { index
                          , credentials:      {username, password}
                          , donationInfo
                          , pinExists
                          , enableSync
                          , userPreferences
                          , userAreaState
                          , cardManagerState
                          , donationLevel
                          , syncDataWire: Just syncDataWire
                          }

  let defaultPagesInfo = Tuple Main $ set _mainPagesState defaultMainState defaultPagesState

  let defaultMessage messageText = WidgetState (spinnerOverlay messageText White) defaultPagesInfo proxyInfo

  let connectionState = {proxy, hashFunc, srpConf, c, p}

  case userAreaEvent of
    (CloseUserAreaEvent) -> 
      (focus "mainView" # liftEffect)
      *> 
      noOperation (Tuple 
                  state
                  (WidgetState
                    hiddenOverlayInfo
                    (pagesInfoWithMain defaultMainState {userAreaState = userAreaState {showUserArea = false, userAreaOpenPage = None}})
                    proxyInfo
                  )
                )

    (OpenUserAreaPage userAreaPage) -> 
      updateUserAreaState defaultMainState userAreaState {userAreaOpenPage = userAreaPage}
    
    (ChangeUserAreaSubmenu userAreaSubmenu) ->
      updateUserAreaState defaultMainState userAreaState {userAreaSubmenus = userAreaSubmenu}
    
    (UpdateDonationLevel days) -> handleDonationPageEvent (DonationEvent.UpdateDonationLevel days) state proxyInfo f

    (UpdateUserPreferencesEvent newUserPreferences) ->
      let pagesInfo = pagesInfoWithMain defaultMainState { userPreferences = newUserPreferences }
          message = spinnerWidgetState pagesInfo
      in do

        liftEffect $ stopTimer
        case (unwrap newUserPreferences).automaticLock of
          Left  _ -> pure unit
          Right n -> liftEffect $ activateTimer n

        newUserInfo                            <- ((\id -> ( set _userInfo_identifier id >>>
                                                                     set _userPreferences newUserPreferences
                                                                   ) userInfo
                                                           ) <$> (computeIdentifier # liftAff)) # (message "Compute Encrypted Data"  # runStep)
        { updateUserInfoOp
        , newMasterKey, newUserInfoReference } <- (computeUserInfoSyncSteps state newUserInfo)  # (message "Compute Sync Operations" # runStep)

        newProxy <- syncBackend      connectionState          updateUserInfoOp           message
        _        <- syncLocalStorage enableSync syncDataWire (updateUserInfoOp <#> fst) (message "Sync Data to Local Storage")

        pure (Tuple 
          (state  { proxy = newProxy
                  , userInfo  = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                  , masterKey = Just newMasterKey
                  })
          (WidgetState hiddenOverlayInfo pagesInfo proxyInfo)
        )
            
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White   

    (ChangePasswordEvent newPassword) ->
      let pagesInfo      = pagesInfoWithMain defaultMainState { credentials = {username, password: newPassword} }
          errorPagesInfo = pagesInfoWithMain defaultMainState                                                    
          message        = spinnerWidgetState pagesInfo
      in do
        { newUserCard, newP } <- (do
          newS <- liftAff $ SRP.randomArrayBuffer 32
          newC <- liftAff $ SRP.prepareC srpConf username newPassword
          newP <- liftAff $ SRP.prepareP srpConf username newPassword

          oldMasterPassword         <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer p)
          masterKeyDecryptedContent <- ExceptT $ decryptArrayBuffer     oldMasterPassword (toArrayBuffer $ (fst masterKey)) <#> (lmap (ProtocolError <<< CryptoError <<< show))
          
          newMasterPassword         <- liftAff $ importCryptoKeyAesGCM  newP
          masterKeyEncryptedContent <- liftAff $ encryptArrayBuffer     newMasterPassword  masterKeyDecryptedContent         <#> fromArrayBuffer
          
          newMasterKey              <- pure    $ Tuple masterKeyEncryptedContent currentMasterKeyEncodingVersion

          newUserCard               <- computeRemoteUserCard srpConf (fromArrayBuffer newC) (fromArrayBuffer newP) (fromArrayBuffer newS) (fst masterKey) newMasterKey

          pure $ { newUserCard, newP: fromArrayBuffer newP }
        ) # (message "Compute New Encrypted Data" # runStep)

        _                     <- (liftEffect $ deleteCredentials =<< localStorage
                                                                 =<< window) # (message "Reset PIN" # runStep)

        let syncOperations =  ( ( Tuple (ChangeUserPassword c newUserCard)
                                        "Change Password"
                                )
                              :   Nil
                              )


        newProxy <- syncBackend      connectionState { c = (unwrap newUserCard).c
                                                     , p =  newP                  } syncOperations           message
        _        <- syncLocalStorage enableSync syncDataWire                       (syncOperations <#> fst) (message "Sync Data to Local Storage")

        pure (Tuple 
          (state  { proxy = newProxy
                  , c = Just (unwrap newUserCard).c, p = Just newP, s = Just (unwrap newUserCard).s
                  , masterKey = Just (unwrap newUserCard).masterKey
                  , password  = Just newPassword
                  , pinExists = false
                  }
          )
          (WidgetState hiddenOverlayInfo pagesInfo proxyInfo)
        )


      # runExceptT
      >>= handleOperationResult state errorPagesInfo true White
    
    (SetPinEvent pinAction) ->
      let pinExists' = case pinAction of
                        Reset    -> false
                        SetPin _ -> true
          pagesInfo = pagesInfoWithMain defaultMainState {pinExists = pinExists'}
      in do
        storage               <- liftEffect $ window >>= localStorage
        _                     <- (case pinAction of
                                    Reset      -> (liftEffect $ deleteCredentials storage)  $> Nothing
                                    SetPin pin -> (saveCredentials state pin storage)      <#> Just
                                  ) # (runStep $ WidgetState
                                                  (spinnerOverlay (case pinAction of
                                                                    Reset    -> "Reset PIN"
                                                                    SetPin _ -> "Set PIN") 
                                                                  White)
                                                  pagesInfo
                                                  proxyInfo 
                                      )
        pure (Tuple 
                (state {pinExists = pinExists'})
                (WidgetState
                  hiddenOverlayInfo
                  pagesInfo
                  proxyInfo
                )
              )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (UpdateSyncPreference enableSync') -> do
      _              <- (updateSyncPreference c enableSync' # liftEffect )     # (defaultMessage "Compute data to sync" # runStep)
      let updatedState = state {enableSync = enableSync'}
      syncOperations <- if enableSync'
                    then
                        (computeSyncOperations   updatedState)                 # (defaultMessage "Compute data to sync" # runStep)
                    else 
                        (computeDeleteOperations updatedState <#> (_ <#> fst)) # (defaultMessage "Compute data to sync" # runStep)
      _              <- (addPendingOperation syncDataWire syncOperations)      # (defaultMessage "Compute data to sync" # runWidgetStep)
      pure (Tuple 
              updatedState
              (WidgetState
                hiddenOverlayInfo
                (pagesInfoWithMain defaultMainState {enableSync = enableSync'})
                proxyInfo
              )
            )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (DeleteAccountEvent) ->do
      syncOperations <- (computeDeleteOperations state                                   ) # ((spinnerWidgetState defaultPagesInfo "Compute data to sync") # runStep)
      _              <-  syncLocalStorage enableSync syncDataWire (syncOperations <#> fst)   ( spinnerWidgetState defaultPagesInfo "Sync Data to Local Storage" )
      _              <- (updateSyncPreference c false # liftEffect                       ) # ((spinnerWidgetState defaultPagesInfo "Delete local data")    # runStep)
      _              <- (liftEffect $ window >>= localStorage >>= deleteCredentials      ) # ((spinnerWidgetState defaultPagesInfo "Delete local data")    # runStep)
      newProxy       <-  syncBackend      connectionState          syncOperations            ( spinnerWidgetState defaultPagesInfo )

      pure $ Tuple 
              (resetState state {proxy = newProxy})
              (WidgetState
                hiddenOverlayInfo
                (pagesInfoWithLogin emptyLoginFormData  { credentials = emptyCredentials
                                                        , loginType   = CredentialLogin
                                                        }
                )
                proxyInfo
              ) 
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White
   
    (ImportCardsEvent importState) ->
      let pagesInfo = pagesInfoWithMain defaultMainState { userAreaState = userAreaState {importState = importState} }
          message messageText = WidgetState (spinnerOverlay messageText White) pagesInfo proxyInfo
      in case importState.step of
        Upload    ->
          do
            result    <-  (case importState.content of
                            Left  file   -> readFile file >>= parseImport
                            Right string -> flip decodeImport string <$> [Epsilon CardVersion_1, Delta] 
                                            # (foldM  (\finalResult singleResult -> do
                                                        if (isRight finalResult) 
                                                        then pure $ finalResult
                                                        else do
                                                          res <- runExceptT singleResult
                                                          if (isRight res)
                                                          then pure $ res
                                                          else pure $ finalResult
                                                      )
                                                      (Left $ ImportError "Invalid input: unable to decode data")
                                              )
                                            # ExceptT
                          ) # (message "Parse Data" # runStep)

            currentDate <- ((((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)) # (message "Get current date" # runStep)

            pure $ Tuple 
                    state $
                    WidgetState
                      hiddenOverlayInfo
                      ( pagesInfoWithMain defaultMainState  { userAreaState = userAreaState 
                                                                              { importState = importState
                                                                                              { content = Right $ stringify $ encode (CA.array currentCardCodecVersion) $ fromCard <$> result
                                                                                              , step = Selection, tag = Tuple true currentDate, selection = result <#> (\card@(DataModel.CardVersions.Card.Card r) -> Tuple (not r.archived) card)
                                                                                              }
                                                                              }
                                                            }
                      ) 
                      proxyInfo

          # runExceptT
          >>= handleOperationResult state pagesInfo true White
      
        Selection ->
          noOperation $ Tuple 
                        state $
                        WidgetState
                          hiddenOverlayInfo
                          (pagesInfoWithMain defaultMainState { userAreaState = userAreaState {importState = importState {step = Confirm}} })
                          proxyInfo
        
        Confirm   ->
          do
            let cardToImport   = filter fst importState.selection <#> snd # ( if fst importState.tag
                                                                              then (map $ addTag (snd importState.tag))
                                                                              else identity
                                                                            )
            let nToImport      = length cardToImport


            (Tuple newCardsCache entries) <- (foldM (\(Tuple cardsCache' entries) card -> do
              Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry hashFuncSHA256 card
              let updatedCardsCache  = insert reference card cardsCache'
              pure $ Tuple updatedCardsCache (List.snoc entries (Tuple cardEntry encryptedCard))
            ) (Tuple cardsCache Nil) cardToImport)                                                                                  # ((spinnerWidgetState pagesInfo "Compute card to import") # runStep)

            newIndex                                            <- (List.foldM (flip addToIndex) index (fst <$> entries) # liftAff) # ((spinnerWidgetState pagesInfo "Update index")           # runStep)
            { updateIndexOp
            , newUserInfo, newUserInfoReference, newMasterKey } <- (computeIndexSyncSteps state newIndex)                           # ((spinnerWidgetState pagesInfo "Compute Encrypted Data") # runStep)


            let syncOperations =  ((mapWithIndex (\i (Tuple cardEntry encryptedCard) -> 
                                      Tuple (SaveBlob (_card_reference  # flip view cardEntry)
                                                      (_card_identifier # flip view cardEntry)
                                                      (encryptedCard    # fromArrayBuffer)
                                            ) 
                                            ("Import card " <> show i <> " of " <> show nToImport)) 
                                  (entries)))
                                  <> updateIndexOp

            newProxy     <-  syncBackend      connectionState          syncOperations          (spinnerWidgetState pagesInfo)
            _            <-  syncLocalStorage enableSync syncDataWire (syncOperations <#> fst) (spinnerWidgetState pagesInfo "Sync Data to Local Storage")

            pure (Tuple 
              (state  { proxy = newProxy
                      , cardsCache = newCardsCache
                      , index = Just newIndex, userInfo = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                      , masterKey = Just newMasterKey
                      }
              )
              (WidgetState
                hiddenOverlayInfo
                ( pagesInfoWithMain defaultMainState  { index            = newIndex
                                                      , userAreaState    = userAreaInitialState
                                                      , cardManagerState = cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                                                      }
                )
                proxyInfo
              )
            )
    
          # runExceptT
          >>= handleOperationResult state pagesInfo true White  

    (ExportEvent OfflineCopy) -> do
        let references              =         userInfoReferences.reference : indexRef : ((\(CardEntry {cardReference: CardReference {reference}}) -> reference) <$> (unwrap index).entries)

        ProxyResponse proxy' blobs <-  downloadBlobsSteps references connectionState defaultPagesInfo proxyInfo
        remoteUserCard             <- (computeRemoteUserCard srpConf c p s (hex "") masterKey)                                                       # (defaultMessage "Compute user card" # runStep)
        ProxyResponse proxy'' doc  <- (getBasicHTML connectionState{proxy = proxy'})                                                                 # (defaultMessage "Download html"     # runStep)
        documentToDownload         <- (appendCardsDataInPlace doc blobs remoteUserCard >>= (liftEffect <<< prepareHTMLBlob))                         # (defaultMessage "Create document"   # runStep)
        date                       <- (formatDateTimeToDate <$> getCurrentDateTime                                                     # liftEffect) # (defaultMessage ""                  # runStep)
        _                          <- (download documentToDownload (date <> "_Clipperz_Offline" <> ".html") "application/octet-stream" # liftEffect) # (defaultMessage "Download document" # runStep)
        pure $ Tuple state {proxy = proxy''} (WidgetState hiddenOverlayInfo defaultPagesInfo proxyInfo)
      
      # runExceptT
      >>= handleOperationResult state defaultPagesInfo true White

    (ExportEvent (UnencryptedCopy exportVersion)) -> do
        ProxyResponse newProxy (Tuple cardsCache' cardList) <-  downloadCardsSteps (unwrap index).entries cardsCache connectionState defaultPagesInfo proxyInfo
        doc                                                 <- (prepareUnencryptedExport exportVersion cardList                                 # liftEffect) # (defaultMessage "Create document"   # runStep)
        date                                                <- (formatDateTimeToDate <$> getCurrentDateTime                                     # liftEffect) # (defaultMessage ""                  # runStep)
        _                                                   <- (download doc (date <> "_Clipperz_Export_"   <> username <> ".html") "text/html" # liftEffect) # (defaultMessage "Download document" # runStep)
                                  
        pure $ Tuple state{proxy = newProxy, cardsCache = cardsCache'} (WidgetState hiddenOverlayInfo defaultPagesInfo proxyInfo)
      # runExceptT
      >>= handleOperationResult state defaultPagesInfo true White

    (LockEvent) -> do
        (logoutSteps state Lock defaultPagesInfo proxyInfo
        # runExceptT
      >>= handleOperationResult state defaultErrorPage true White)
      <* (eventDelayed (focus "loginPassphraseInput" *> focus "loginPINInput") # affAction)
  
    (LogoutEvent) -> do
        (logoutSteps state Logout defaultPagesInfo proxyInfo
        # runExceptT
      >>= handleOperationResult state defaultErrorPage true White)
      <* (eventDelayed (focus "loginUsernameInput" *> focus "loginPINInput") # affAction)

  where
    spinnerWidgetState :: Tuple Page PagesState -> String -> WidgetState
    spinnerWidgetState pagesInfo message = WidgetState (spinnerOverlay message White) pagesInfo proxyInfo

    updateUserAreaState :: MainPageWidgetState -> UserAreaState -> Widget HTML OperationState
    updateUserAreaState mainPageState userAreaState' = 
      noOperation (Tuple 
                    state
                    (WidgetState
                      hiddenOverlayInfo
                      (pagesInfoWithMain mainPageState { userAreaState = userAreaState' })
                      proxyInfo
                    )
                  )

handleUserAreaEvent _ _ _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "userAreaEvent")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

-- ===================================================================================================

downloadCardsSteps :: List CardEntry -> CardsCache -> ConnectionState -> Tuple Page PagesState -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache (List Card)))
downloadCardsSteps cardEntryList cardsCache connectionState pagesInfo proxyInfo = 
  foldWithIndexM (\i (ProxyResponse proxy' (Tuple cardsCache' cards)) cardEntry -> do
    ProxyResponse proxy'' (Tuple cardsCache'' card) <- (getCard connectionState{proxy = proxy'} cardsCache' cardEntry) # (runStep $ WidgetState (spinnerOverlay ("Download card " <> show i <> " of " <> show nToDownload) White) pagesInfo proxyInfo)
    pure $ ProxyResponse proxy'' (Tuple cardsCache'' (List.snoc cards card))
  ) (ProxyResponse connectionState.proxy (Tuple cardsCache Nil)) cardEntryList
  where
    nToDownload = List.length cardEntryList

downloadBlobsSteps :: List HexString -> ConnectionState -> Tuple Page PagesState -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse BlobsList)
downloadBlobsSteps referenceList connectionState pagesInfo proxyInfo = 
  foldWithIndexM (\i (ProxyResponse proxy' blobs) reference -> do
    ProxyResponse proxy'' arrayBuffer <- (getBlob connectionState{proxy = proxy'} reference) # (runStep $ WidgetState (spinnerOverlay ("Download blob " <> show i <> " of " <> show nToDownload) White) pagesInfo proxyInfo)
    pure $ ProxyResponse proxy'' (List.snoc blobs (Tuple reference (fromArrayBuffer arrayBuffer)))
  ) (ProxyResponse connectionState.proxy Nil) referenceList
  where
    nToDownload = List.length referenceList

data LogoutType = Lock | Logout

logoutTypeMessage :: LogoutType -> String
logoutTypeMessage Lock   = "Lock"
logoutTypeMessage Logout = "Logout"

logoutTypeUserame :: Maybe Username -> LogoutType -> Username
logoutTypeUserame username = case _ of
  Lock   -> fromMaybe "" username
  Logout ->           ""

logoutSteps :: AppState -> LogoutType -> Tuple Page PagesState -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
logoutSteps state@{username, hash: hashFunc, proxy, srpConf} logoutType pagesInfo proxyInfo =
  let message = WidgetState (spinnerOverlay (logoutTypeMessage logoutType) White) pagesInfo proxyInfo
  in do
    proxy' <- case proxy of
      DynamicProxy (OfflineProxy _ _) -> pure $ DynamicProxy $ OfflineProxy Nothing NoData
      _ -> do 
        let connectionState = {proxy, hashFunc, srpConf, c: hex "", p: hex ""}
        _ <- (genericRequest connectionState "logout" POST Nothing RF.ignore) # (runStep $ message)
        pure $ DynamicProxy defaultOnlineProxy

    stopTimer # liftEffect

    localStorageUsername <- (liftEffect $ window >>= localStorage >>= getItem pinUsernameKey) # (runStep $ message)    
    pinExists            <-  pinExists # liftEffect

    pure $ Tuple 
            ((resetState state) {pinExists =pinExists , proxy = proxy'})
            (WidgetState
              hiddenOverlayInfo
              ( pagesInfoWithLogin emptyLoginFormData { credentials = emptyCredentials {username = logoutTypeUserame username logoutType}
                                                      , loginType   = loginType localStorageUsername pinExists
                                                      }
              )
              proxyInfo
            ) 
  where
    loginType :: Maybe String -> Boolean -> LoginType
    loginType localStorageUsername pinExists =
      if pinExists
      then
        case logoutType of
          Lock   -> if (localStorageUsername == username) then PinLogin else CredentialLogin
          Logout -> PinLogin
      else CredentialLogin