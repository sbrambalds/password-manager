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
import DataModel.WidgetState (CardManagerState, CardViewState(..), ImportStep(..), LoginType(..), MainPageWidgetState, Page(..), UserAreaPage(..), UserAreaState, WidgetState(..))
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
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, runWidgetStep, syncLocalStorage)
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
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SetPinView (PinEvent(..))
import Views.UserAreaView (UserAreaEvent(..), userAreaInitialState)
import Web.DownloadJs (download)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState


handleUserAreaEvent userAreaEvent cardManagerState userAreaState state@{proxy, srpConf, hash: hashFunc, cardsCache, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {indexReference: IndexReference { reference: indexRef}, userPreferences, donationInfo}), userInfoReferences: Just userInfoReferences, c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, pinExists, enableSync, donationLevel: Just donationLevel, syncDataWire} proxyInfo f = do
  let defaultPage = { index
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
  
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  case userAreaEvent of
    (CloseUserAreaEvent) -> 
      (focus "mainView" # liftEffect)
      *> 
      noOperation (Tuple 
                  state
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage {userAreaState = userAreaState {showUserArea = false, userAreaOpenPage = None}})
                    proxyInfo
                  )
                )

    (OpenUserAreaPage userAreaPage) -> 
      updateUserAreaState defaultPage userAreaState {userAreaOpenPage = userAreaPage}
    
    (ChangeUserAreaSubmenu userAreaSubmenu) ->
      updateUserAreaState defaultPage userAreaState {userAreaSubmenus = userAreaSubmenu}
    
    (UpdateDonationLevel days) -> handleDonationPageEvent (DonationEvent.UpdateDonationLevel days) state proxyInfo f

    (UpdateUserPreferencesEvent newUserPreferences) ->
      let page = Main defaultPage { userPreferences = newUserPreferences }
          message = spinnerWidgetState page
      in do

        liftEffect $ stopTimer
        case (unwrap newUserPreferences).automaticLock of
          Left  _ -> pure unit
          Right n -> liftEffect $ activateTimer n

        newUserInfo                            <- runStep ((\id -> ( set _userInfo_identifier id >>>
                                                                     set _userPreferences newUserPreferences
                                                                   ) userInfo
                                                           ) <$> (computeIdentifier # liftAff))        (message "Compute Encrypted Data")
        { updateUserInfoOp
        , newMasterKey, newUserInfoReference } <- runStep (computeUserInfoSyncSteps state newUserInfo) (message "Compute Sync Operations")

        newProxy <- syncBackend      connectionState          updateUserInfoOp           message
        _        <- syncLocalStorage enableSync syncDataWire (updateUserInfoOp <#> fst) (message "Sync Data to Local Storage")

        pure (Tuple 
          (state  { proxy = newProxy
                  , userInfo  = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                  , masterKey = Just newMasterKey
                  })
          (WidgetState hiddenOverlayInfo page proxyInfo)
        )
            
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White   

    (ChangePasswordEvent newPassword) ->
      let page      = Main defaultPage { credentials = {username, password: newPassword} }
          errorPage = Main defaultPage
          message   = spinnerWidgetState page
      in do
        { newUserCard, newP } <- runStep (do
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
        ) (message "Compute New Encrypted Data")

        _                     <- runStep (liftEffect $ deleteCredentials =<< localStorage
                                                                         =<< window)       (message "Reset PIN")

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
                  , masterKey = Just masterKey
                  , password  = Just newPassword
                  , pinExists = false
                  }
          )
          (WidgetState hiddenOverlayInfo page proxyInfo)
        )


      # runExceptT
      >>= handleOperationResult state errorPage true White
    
    (SetPinEvent pinAction) ->
      let pinExists' = case pinAction of
                        Reset    -> false
                        SetPin _ -> true
          page = Main defaultPage {pinExists = pinExists'}
      in do
        storage               <- liftEffect $ window >>= localStorage
        _                     <- runStep (case pinAction of
                                          Reset      -> (liftEffect $ deleteCredentials storage)  $> Nothing
                                          SetPin pin -> (saveCredentials state pin storage)      <#> Just
                                        ) (WidgetState
                                            (spinnerOverlay (case pinAction of
                                                              Reset    -> "Reset PIN"
                                                              SetPin _ -> "Set PIN") 
                                                            White)
                                            page
                                            proxyInfo
                                          )
        pure (Tuple 
                (state {pinExists = pinExists'})
                (WidgetState
                  hiddenOverlayInfo
                  page
                  proxyInfo
                )
              )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (UpdateSyncPreference enableSync') -> do
      _              <- runStep       (updateSyncPreference c enableSync' # liftEffect ) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
      let updatedState = state {enableSync = enableSync'}
      syncOperations <- if enableSync'
                     then
                        runStep       (computeSyncOperations   updatedState            ) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
                     else 
                        runStep       (computeDeleteOperations updatedState <#> (_ <#> fst)) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
      _              <- runWidgetStep (addPendingOperation syncDataWire syncOperations) (WidgetState {status: Spinner, color: Black, message: "Compute data to sync"} (Main defaultPage) proxyInfo)
      pure (Tuple 
              updatedState
              (WidgetState
                hiddenOverlayInfo
                (Main defaultPage {enableSync = enableSync'})
                proxyInfo
              )
            )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (DeleteAccountEvent) ->
      let page = Main defaultPage
      in do
        syncOperations <- (computeDeleteOperations state                                   ) # ((spinnerWidgetState page "Compute data to sync") # flip runStep)
        _              <-  syncLocalStorage enableSync syncDataWire (syncOperations <#> fst)   ( spinnerWidgetState page "Sync Data to Local Storage" )
        _              <- (updateSyncPreference c false # liftEffect                       ) # ((spinnerWidgetState page "Delete local data")    # flip runStep)
        _              <- (liftEffect $ window >>= localStorage >>= deleteCredentials      ) # ((spinnerWidgetState page "Delete local data")    # flip runStep)
        newProxy       <-  syncBackend      connectionState          syncOperations            ( spinnerWidgetState page )
        
        pure $ Tuple 
                (resetState state {proxy = newProxy})
                (WidgetState
                  hiddenOverlayInfo
                  (Login emptyLoginFormData { credentials = emptyCredentials
                                            , loginType   = CredentialLogin
                                            }
                  )
                  proxyInfo
                ) 
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White
   
    (ImportCardsEvent importState) ->
      let page = Main defaultPage { userAreaState = userAreaState {importState = importState} }
      in case importState.step of
        Upload    ->
          do
            result    <- runStep  (case importState.content of
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
                                  ) (WidgetState (spinnerOverlay "Parse Data" White) page proxyInfo)

            currentDate <- runStep ((((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)) (WidgetState (spinnerOverlay "Get current date" White) page proxyInfo)

            pure $ Tuple 
                    state $
                    WidgetState
                      hiddenOverlayInfo
                      (Main defaultPage  { userAreaState = userAreaState 
                                                            { importState = importState
                                                                              { content = Right $ stringify $ encode (CA.array currentCardCodecVersion) $ fromCard <$> result
                                                                              , step = Selection, tag = Tuple true currentDate, selection = result <#> (\card@(DataModel.CardVersions.Card.Card r) -> Tuple (not r.archived) card)
                                                                              }
                                                            }
                                        }
                      ) 
                      proxyInfo

          # runExceptT
          >>= handleOperationResult state page true White
      
        Selection ->
          noOperation $ Tuple 
                        state $
                        WidgetState
                          hiddenOverlayInfo
                          (Main defaultPage { userAreaState = userAreaState {importState = importState {step = Confirm}} })
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
            ) (Tuple cardsCache Nil) cardToImport)                                                                                  # ((spinnerWidgetState page "Compute card to import") # flip runStep)

            newIndex                                            <- (List.foldM (flip addToIndex) index (fst <$> entries) # liftAff) # ((spinnerWidgetState page "Update index")           # flip runStep)
            { updateIndexOp
            , newUserInfo, newUserInfoReference, newMasterKey } <- (computeIndexSyncSteps state newIndex)                           # ((spinnerWidgetState page "Compute Encrypted Data") # flip runStep)


            let syncOperations =  ((mapWithIndex (\i (Tuple cardEntry encryptedCard) -> 
                                      Tuple (SaveBlob (_card_reference  # flip view cardEntry)
                                                      (_card_identifier # flip view cardEntry)
                                                      (encryptedCard    # fromArrayBuffer)
                                            ) 
                                            ("Import card " <> show i <> " of " <> show nToImport)) 
                                  (entries)))
                                  <> updateIndexOp

            newProxy     <-  syncBackend      connectionState          syncOperations          (spinnerWidgetState page)
            _            <-  syncLocalStorage enableSync syncDataWire (syncOperations <#> fst) (spinnerWidgetState page "Sync Data to Local Storage")

            pure (Tuple 
              (state  { proxy = newProxy
                      , cardsCache = newCardsCache
                      , index = Just newIndex, userInfo = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                      , masterKey = Just newMasterKey
                      }
              )
              (WidgetState
                hiddenOverlayInfo
                (Main defaultPage { index            = newIndex
                                  , userAreaState    = userAreaInitialState
                                  , cardManagerState = cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                                  }
                )
                proxyInfo
              )
          )
      
          # runExceptT
          >>= handleOperationResult state page true White  

    (ExportEvent OfflineCopy) ->
      let page = Main defaultPage
      in do
        let references              =         userInfoReferences.reference : indexRef : ((\(CardEntry {cardReference: CardReference {reference}}) -> reference) <$> (unwrap index).entries)

        ProxyResponse proxy' blobs <-          downloadBlobsSteps references connectionState page proxyInfo
        remoteUserCard             <- runStep (computeRemoteUserCard srpConf c p s (hex "") masterKey)                                                                                  (WidgetState (spinnerOverlay "Compute user card" White) page proxyInfo)
        ProxyResponse proxy'' doc  <- runStep (getBasicHTML connectionState{proxy = proxy'})                                                                 (WidgetState (spinnerOverlay "Download html"     White) page proxyInfo)
        documentToDownload         <- runStep (appendCardsDataInPlace doc blobs remoteUserCard >>= (liftEffect <<< prepareHTMLBlob))                         (WidgetState (spinnerOverlay "Create document"   White) page proxyInfo)
        date                       <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                                     (WidgetState (spinnerOverlay ""                  White) page proxyInfo)
        _                          <- runStep (liftEffect $ download documentToDownload (date <> "_Clipperz_Offline" <> ".html") "application/octet-stream") (WidgetState (spinnerOverlay "Download document" White) page proxyInfo)
        pure $ Tuple state {proxy = proxy''} (WidgetState hiddenOverlayInfo page proxyInfo)
      
      # runExceptT
      >>= handleOperationResult state page true White

    (ExportEvent (UnencryptedCopy exportVersion)) ->
      let page = Main defaultPage
      in do
        ProxyResponse newProxy (Tuple cardsCache' cardList) <-                       downloadCardsSteps (unwrap index).entries cardsCache connectionState page proxyInfo
        doc                                                 <- runStep (liftEffect $ prepareUnencryptedExport exportVersion cardList)                                             (WidgetState (spinnerOverlay "Create document"   White) page proxyInfo)
        date                                                <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                   (WidgetState (spinnerOverlay ""                  White) page proxyInfo)
        _                                                   <- runStep (liftEffect $ download doc (date <> "_Clipperz_Export_"   <> username <> ".html") "text/html") (WidgetState (spinnerOverlay "Download document" White) page proxyInfo)
                                  
        pure $ Tuple state{proxy = newProxy, cardsCache = cardsCache'} (WidgetState hiddenOverlayInfo page proxyInfo)
      # runExceptT
      >>= handleOperationResult state page true White

    (LockEvent) ->
      let page = Main defaultPage
      in
        (logoutSteps state Lock page proxyInfo
        # runExceptT
        >>= handleOperationResult state defaultErrorPage true White)
        <* (eventDelayed (focus "loginPassphraseInput" *> focus "loginPINInput") # affAction)
  
    (LogoutEvent) ->
      let page = Main defaultPage
      in 
        (logoutSteps state Logout page proxyInfo
        # runExceptT
        >>= handleOperationResult state defaultErrorPage true White)
        <* (eventDelayed (focus "loginUsernameInput" *> focus "loginPINInput") # affAction)

  where
    spinnerWidgetState :: Page -> String -> WidgetState
    spinnerWidgetState page message = WidgetState (spinnerOverlay message White) page proxyInfo

    updateUserAreaState :: MainPageWidgetState -> UserAreaState -> Widget HTML OperationState
    updateUserAreaState mainPageState userAreaState' = 
      noOperation (Tuple 
                    state
                    (WidgetState
                      hiddenOverlayInfo
                      (Main mainPageState { userAreaState = userAreaState' }
                      )
                      proxyInfo
                    )
                  )

handleUserAreaEvent _ _ _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "userAreaEvent")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

-- ===================================================================================================

downloadCardsSteps :: List CardEntry -> CardsCache -> ConnectionState -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache (List Card)))
downloadCardsSteps cardEntryList cardsCache connectionState page proxyInfo = 
  foldWithIndexM (\i (ProxyResponse proxy' (Tuple cardsCache' cards)) cardEntry -> do
    ProxyResponse proxy'' (Tuple cardsCache'' card) <- runStep (getCard connectionState{proxy = proxy'} cardsCache' cardEntry) (WidgetState (spinnerOverlay ("Download card " <> show i <> " of " <> show nToDownload) White) page proxyInfo)
    pure $ ProxyResponse proxy'' (Tuple cardsCache'' (List.snoc cards card))
  ) (ProxyResponse connectionState.proxy (Tuple cardsCache Nil)) cardEntryList
  where
    nToDownload = List.length cardEntryList

downloadBlobsSteps :: List HexString -> ConnectionState -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) (ProxyResponse BlobsList)
downloadBlobsSteps referenceList connectionState page proxyInfo = 
  foldWithIndexM (\i (ProxyResponse proxy' blobs) reference -> do
    ProxyResponse proxy'' arrayBuffer <- runStep (getBlob connectionState{proxy = proxy'} reference) (WidgetState (spinnerOverlay ("Download blob " <> show i <> " of " <> show nToDownload) White) page proxyInfo)
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

logoutSteps :: AppState -> LogoutType -> Page -> ProxyInfo -> ExceptT AppError (Widget HTML) OperationState
logoutSteps state@{username, hash: hashFunc, proxy, srpConf} logoutType page proxyInfo =
  do
    let message = logoutTypeMessage logoutType
    proxy' <- case proxy of
      DynamicProxy (OfflineProxy _ _) -> pure $ DynamicProxy $ OfflineProxy Nothing NoData
      _ -> do 
        let connectionState = {proxy, hashFunc, srpConf, c: hex "", p: hex ""}
        _ <- runStep (genericRequest connectionState "logout" POST Nothing RF.ignore) (WidgetState (spinnerOverlay message White) page proxyInfo)
        pure $ DynamicProxy defaultOnlineProxy

    stopTimer # liftEffect

    localStorageUsername <- runStep (liftEffect $ window >>= localStorage >>= getItem pinUsernameKey)       (WidgetState (spinnerOverlay message White) page proxyInfo)    
    pinExists            <- pinExists # liftEffect

    pure $ Tuple 
            ((resetState state) {pinExists =pinExists , proxy = proxy'})
            (WidgetState
              hiddenOverlayInfo
              (Login emptyLoginFormData { credentials = emptyCredentials {username = logoutTypeUserame username logoutType}
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