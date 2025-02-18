module Functions.Handler.DonationEventHandler
  ( handleDonationPageEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML, affAction)
import Control.Alternative (pure, (*>))
import Control.Applicative ((<#>))
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.DateTime (adjust)
import Data.Function ((#), ($))
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.FragmentState as Fragment
import DataModel.Proxy (ProxyInfo)
import DataModel.UserVersions.User (UserInfo(..))
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), Page(..), PagesState, WidgetState(..), _mainPagesState)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Communication.SyncBackend (syncBackend)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.Events (effectDelayed, focus)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, defaultPagesState, handleOperationResult, noOperation, runStep, syncLocalStorage)
import Functions.User (computeUserInfoSyncSteps)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.CreateCardView (emptyCardFormData)
import Views.DonationViews (DonationPageEvent(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleDonationPageEvent :: DonationPageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState

handleDonationPageEvent donationPageEvent state@{c: Just c, p: Just p, srpConf, proxy, hash: hashFunc, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), pinExists, passkeyExists, enableSync, syncDataWire, donationLevel: Just donationLevel} proxyInfo fragmentState = do
  let defaultMainState = { index
                        , credentials:      {username, password}
                        , donationInfo
                        , pinExists
                        , passkeyExists
                        , enableSync
                        , userPreferences
                        , userAreaState:    userAreaInitialState
                        , cardManagerState: cardManagerInitialState
                        , donationLevel
                        , syncDataWire: Just syncDataWire
                        }
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  let defaultPagesInfo = Tuple Main $ set _mainPagesState defaultMainState defaultPagesState

  case donationPageEvent of
    UpdateDonationLevel days  ->
      do
        let page = Tuple Main $ set _mainPagesState defaultMainState { donationLevel = DonationOk } defaultPagesState

        newUserInfo                            <- ((\now -> pure $ UserInfo ((unwrap userInfo) {donationInfo = do
                                                    nextDonationReminder <- adjust days now
                                                    pure {dateOfLastDonation: now, nextDonationReminder}})
                                                  ) =<< liftEffect nowDateTime)                         # (spinnerWidgetState page "Update user info"        # runStep)
        newDonationLevel                       <- (computeDonationLevel index newUserInfo # liftEffect) # (spinnerWidgetState page "Update user info"        # runStep)
        
        { updateUserInfoOp
        , newMasterKey, newUserInfoReference } <- (computeUserInfoSyncSteps state newUserInfo)          # (spinnerWidgetState page "Compute Sync Operations" # runStep)

        newProxy <- syncBackend      connectionState          updateUserInfoOp          (spinnerWidgetState page)
        _        <- syncLocalStorage enableSync syncDataWire (updateUserInfoOp <#> fst) (spinnerWidgetState page "Sync Data to Local Storage")

        let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (emptyCardFormData {card = card}) (NewCardFromFragment card)
                        _                     -> NoCard
        focus "mainView" # liftEffect
        pure (Tuple 
          (state  { proxy = newProxy
                  , userInfo  = Just newUserInfo, userInfoReferences = Just newUserInfoReference
                  , masterKey = Just newMasterKey
                  , donationLevel = Just newDonationLevel
                  })
          (WidgetState
            hiddenOverlayInfo
            (Tuple Main $
                  set _mainPagesState 
                      emptyMainPageWidgetState  { index            = index
                                                , cardManagerState = cardManagerInitialState { cardViewState = cardViewState }
                                                , donationLevel    = newDonationLevel
                                                }
                      defaultPagesState
                  
            )
            proxyInfo
          )
        )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    CloseDonationPage -> 
      (effectDelayed 10.0 (focus "mainView" # liftEffect) # affAction) *>
      (noOperation (Tuple state $ WidgetState hiddenOverlayInfo defaultPagesInfo proxyInfo))
  where
    spinnerWidgetState :: Tuple Page PagesState -> String -> WidgetState
    spinnerWidgetState pagesInfo message = WidgetState (spinnerOverlay message Black) pagesInfo proxyInfo

handleDonationPageEvent _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "DonationPage")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White