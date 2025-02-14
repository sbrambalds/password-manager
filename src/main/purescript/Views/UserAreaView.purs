module Views.UserAreaView where

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, header, li, li', span, text, ul)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Bind (bind, (=<<))
import Control.Category ((>>>))
import Data.Eq ((==))
import Data.Formatter.DateTime (format)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days)
import Data.Tuple (Tuple(..), swap)
import DataModel.Credentials (Credentials)
import DataModel.Proxy (ProxyInfo(..))
import DataModel.UserVersions.User (UserPreferences, DonationInfo)
import DataModel.UserVersions.UserCodecs (iso8601DateFormatter)
import DataModel.WidgetState (ImportState, UserAreaPage(..), UserAreaState, UserAreaSubmenu(..))
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit, donationIFrameURL)
import OperationalWidgets.Sync (SyncData)
import Unsafe.Coerce (unsafeCoerce)
import Views.ChangePasswordView (changePasswordView)
import Views.Components (Enabled(..), footerComponent)
import Views.DeleteUserView (deleteUserView)
import Views.DeviceSyncView (EnableSync, deviceSyncView)
import Views.DonationViews (donationIFrame)
import Views.DonationViews as DonationEvent
import Views.ExportView (ExportEvent, exportView)
import Views.ImportView (importView, initialImportState)
import Views.SetPasskeyView (PasskeyEvent, setPasskeyView)
import Views.SetPinView (PinEvent, setPinView)
import Views.UserPreferencesView (userPreferencesView)

data UserAreaEvent    = CloseUserAreaEvent
                      | ChangeUserAreaSubmenu (Map UserAreaSubmenu Boolean)
                      | OpenUserAreaPage UserAreaPage
                      | UpdateUserPreferencesEvent UserPreferences
                      | ChangePasswordEvent String
                      | SetPinEvent PinEvent
                      | SetPasskeyEvent PasskeyEvent
                      | DeleteAccountEvent
                      | ImportCardsEvent ImportState
                      | ExportEvent ExportEvent
                      | UpdateDonationLevel Days
                      | UpdateSyncPreference Boolean
                      | LockEvent
                      | LogoutEvent

userAreaInitialState :: UserAreaState
userAreaInitialState = { showUserArea: false, userAreaOpenPage: None, importState: initialImportState, userAreaSubmenus: fromFoldable [(Tuple Account false), (Tuple Data false)]}

userAreaView :: UserAreaState -> UserPreferences -> Credentials -> Maybe DonationInfo -> ProxyInfo -> Boolean -> Boolean -> EnableSync -> Maybe (Wire (Widget HTML) SyncData) -> Widget HTML (Tuple UserAreaEvent UserAreaState)
userAreaView state@{showUserArea, userAreaOpenPage, importState, userAreaSubmenus} userPreferences credentials donationInfo proxyInfo pinExists passkeyExists enableSync syncDataWire = do
  commitHash <- liftEffect currentCommit
  ((div [Props._id "userPage", Props.className (if showUserArea then "open" else "closed"), Props.tabIndex 0, Props.filterProp (\e -> (unsafeCoerce e).key == "Escape") Props.onKeyDown $> CloseUserAreaEvent] [
      div [Props.onClick, Props.className "mask"] [] $> CloseUserAreaEvent
    , div [Props.className "panel"] [
        header [] [div [] [button [Props.onClick] [text "menu"]]] $> CloseUserAreaEvent
      , userAreaMenu (proxyInfo == Online)
      , footerComponent commitHash
      ]
    , userAreaInternalView
    ])
  ) <#> (Tuple state >>> swap)

  where
    userAreaMenu :: Boolean -> Widget HTML UserAreaEvent
    userAreaMenu enabled =
      ul [Props._id "userSidebar"] [
        subMenu Account "Account" [
          subMenuElement Preferences    (Enabled enabled) "Preferences"
        , subMenuElement ChangePassword (Enabled enabled) "Passphrase"
        , subMenuElement Delete         (Enabled enabled) "Delete account"
        ]
      , subMenu Device  "Device" [
          subMenuElement Pin            (Enabled true   ) "PIN"
        , subMenuElement Passkey        (Enabled true   ) "Passkey"
        , subMenuElement DeviceSync     (Enabled true   ) "Sync" 
      ]
      , subMenu Data    "Data"    [
          subMenuElement Import         (Enabled enabled) "Import"
        , subMenuElement Export         (Enabled enabled) "Export"
        ]
      , subMenuElement   Donate         (Enabled true)     "Donate" <#> OpenUserAreaPage
      , li' [a      [Props.className "link", Props.href "/about/app", Props.target "_blank"] [span [] [text "About"]]]
      , li' [button [Props.onClick, Props._id "lockButton"]                                  [span [] [text "Lock"]]]   $> LockEvent
      , li' [button [Props.onClick]                                                          [span [] [text "Logout"]]] $> LogoutEvent
      ]

      where
        subMenu :: UserAreaSubmenu -> String -> Array (Widget HTML UserAreaPage) -> Widget HTML UserAreaEvent
        subMenu userAreaSubmenu label subMenuElements = li [] [
          button [Props.onClick] [span [] [text label]] $> ChangeUserAreaSubmenu (invertSubmenuValue userAreaSubmenu)
        , ul [Props.classList [Just "userSidebarSubitems", if isSubmenuOpen userAreaSubmenu then Nothing else Just "hidden"]]
            subMenuElements <#> OpenUserAreaPage
        ]

        subMenuElement :: UserAreaPage -> Enabled -> String -> Widget HTML UserAreaPage
        subMenuElement userAreaPage (Enabled enabled') label = li [Props.classList [if userAreaOpenPage == userAreaPage then Just "selected" else Nothing, Just "subMenuElement"]] [
          button [Props.onClick, Props.disabled (not enabled')] [span [] [text label]] $> (if userAreaOpenPage == userAreaPage then None else userAreaPage)
        ]
        
        invertSubmenuValue :: UserAreaSubmenu -> Map UserAreaSubmenu Boolean
        invertSubmenuValue userAreaSubmenu = insert userAreaSubmenu (not $ isSubmenuOpen userAreaSubmenu) userAreaSubmenus

        isSubmenuOpen :: UserAreaSubmenu -> Boolean
        isSubmenuOpen userAreaSubmenu = fromMaybe false $ lookup userAreaSubmenu userAreaSubmenus

    userAreaInternalView :: Widget HTML UserAreaEvent
    userAreaInternalView = 
      case userAreaOpenPage of
        Preferences     -> frame (userPreferencesView userPreferences <#> UpdateUserPreferencesEvent)
        ChangePassword  -> frame (changePasswordView  credentials     <#> ChangePasswordEvent)
        Delete          -> frame (deleteUserView      credentials      $> DeleteAccountEvent)
        Pin             -> frame (setPinView          pinExists       <#> SetPinEvent)
        Passkey         -> frame (setPasskeyView      passkeyExists   <#> SetPasskeyEvent)
        DeviceSync      -> frame (deviceSyncView      enableSync 
                                                      proxyInfo
                                                      syncDataWire    <#> UpdateSyncPreference)
        Import          -> frame (importView          importState     <#> ImportCardsEvent)
        Export          -> frame (exportView                          <#> ExportEvent)
        Donate          -> frame (donationUserArea)     
        About           -> frame (text "This is Clipperz")
        None            -> emptyUserComponent

      where
        frame :: Widget HTML UserAreaEvent -> Widget HTML UserAreaEvent
        frame c = div [Props.classList $ Just <$> ["extraFeatureContent", "open"]] [
          header [] [div [] [button [Props.onClick] [text "close"]]] $> OpenUserAreaPage None
        , c
        ]

        emptyUserComponent :: forall a. Widget HTML a
        emptyUserComponent = (div [Props.className "extraFeatureContent"] [])

        donationUserArea :: Widget HTML UserAreaEvent
        donationUserArea = 
          div [Props._id "donationUserArea"] [
            div [Props.className "donationInfo"] $ case donationInfo of
              Just {dateOfLastDonation, nextDonationReminder} -> [
                span [Props.className "dateOfLastDonation"] [text $ format iso8601DateFormatter dateOfLastDonation]
              , span [Props.className "dateOfNextReminder"] [text $ format iso8601DateFormatter nextDonationReminder]
              ]
              Nothing -> [ text "We don't have any donation bound to this account 🥹" ]
          , donationIFrame =<< (liftEffect $ donationIFrameURL "userarea/")
          ] <#> 
          (\res -> case res of
            DonationEvent.CloseDonationPage     ->  OpenUserAreaPage None
            DonationEvent.UpdateDonationLevel m ->  UpdateDonationLevel m
          )
