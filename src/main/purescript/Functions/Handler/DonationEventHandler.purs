module Functions.Handler.DonationEventHandler
  ( handleDonationPageEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alternative (pure, (*>))
import Control.Applicative ((<#>))
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.DateTime (adjust)
import Data.Function ((#), ($))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst)
import DataModel.AppError (AppError(..), InvalidStateError(..))
import DataModel.AppState (AppState)
import DataModel.FragmentState as Fragment
import DataModel.Proxy (ProxyInfo)
import DataModel.UserVersions.User (UserInfo(..))
import DataModel.WidgetState (CardFormInput(..), CardViewState(..), Page(..), WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Functions.Communication.SyncBackend (syncBackend)
import Functions.Donations (DonationLevel(..), computeDonationLevel)
import Functions.Events (focus)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, handleOperationResult, noOperation, runStep, syncLocalStorage)
import Functions.User (computeUserInfoSyncSteps)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (cardManagerInitialState)
import Views.CreateCardView (emptyCardFormData)
import Views.DonationViews (DonationPageEvent(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

handleDonationPageEvent :: DonationPageEvent -> AppState -> ProxyInfo -> Fragment.FragmentState -> Widget HTML OperationState

handleDonationPageEvent donationPageEvent state@{c: Just c, p: Just p, srpConf, proxy, hash: hashFunc, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {userPreferences, donationInfo}), pinExists, enableSync, syncDataWire, donationLevel: Just donationLevel} proxyInfo fragmentState = do
  let defaultPage = { index
                    , credentials:      {username, password}
                    , donationInfo
                    , pinExists
                    , enableSync
                    , userPreferences
                    , userAreaState:    userAreaInitialState
                    , cardManagerState: cardManagerInitialState
                    , donationLevel
                    , syncDataWire: Just syncDataWire
                    }
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  case donationPageEvent of
    UpdateDonationLevel days  ->
      do
        let page = Main defaultPage { donationLevel = DonationOk }

        newUserInfo                     <- runStep ((\now -> pure $ UserInfo ((unwrap userInfo) {donationInfo = do
                                                      nextDonationReminder <- adjust days now
                                                      pure {dateOfLastDonation: now, nextDonationReminder}})
                                                    ) =<< liftEffect nowDateTime)                        (spinnerWidgetState page "Update user info")
        newDonationLevel                <- runStep (computeDonationLevel index newUserInfo # liftEffect) (spinnerWidgetState page "Update user info")
        
        { updateUserInfoOp
        , newMasterKey, newUserInfoReference }    <- runStep (computeUserInfoSyncSteps state newUserInfo) (spinnerWidgetState page "Compute Sync Operations")


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
            (Main emptyMainPageWidgetState  { index            = index
                                            , cardManagerState = cardManagerInitialState { cardViewState = cardViewState }
                                            , donationLevel    = newDonationLevel
                                            }
            )
            proxyInfo
          )
        )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    CloseDonationPage -> (focus "mainView" # liftEffect) *> noOperation (Tuple state $ WidgetState hiddenOverlayInfo (Main defaultPage) proxyInfo)
  where
    spinnerWidgetState :: Page -> String -> WidgetState
    spinnerWidgetState page message = WidgetState (spinnerOverlay message Black) page proxyInfo

handleDonationPageEvent _ state _ _ = do
  throwError $ InvalidStateError (CorruptedState "DonationPage")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White