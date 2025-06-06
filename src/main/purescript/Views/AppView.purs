module Views.AppView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, h1, h3, header, li, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Bind (bind)
import Data.Eq ((==))
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..), uncurry)
import DataModel.Credentials (emptyCredentials)
import DataModel.IndexVersions.Index (emptyIndex)
import DataModel.Proxy (ProxyInfo)
import DataModel.UserVersions.User (defaultUserPreferences)
import DataModel.WidgetState (MainPageWidgetState, Page(..), UserAreaState, WidgetState(..), CardManagerState)
import Effect.Class (liftEffect)
import Functions.Donations (DonationLevel(..))
import Functions.EnvironmentalVariables (currentCommit)
import Test.Debug (debugState)
import Views.CardsManagerView (CardManagerEvent, cardManagerInitialState, cardsManagerView)
import Views.Components (Enabled(..), footerComponent, proxyInfoComponent)
import Views.DonationViews (DonationPageEvent, donationPage)
import Views.LoginFormView (LoginPageEvent, loginPage)
import Views.OverlayView (overlay)
import Views.SignupFormView (SignupPageEvent, signupFormView)
import Views.UserAreaView (UserAreaEvent, userAreaInitialState, userAreaView)

emptyMainPageWidgetState :: MainPageWidgetState
emptyMainPageWidgetState = { index: emptyIndex, credentials: emptyCredentials, donationInfo: Nothing, pinExists: false, passkeyExists: false, userAreaState: userAreaInitialState, cardManagerState: cardManagerInitialState, donationLevel: DonationOk, userPreferences: defaultUserPreferences, enableSync: false, syncDataWire: Nothing }

data PageEvent = LoginPageEvent           LoginPageEvent
               | SignupPageEvent          SignupPageEvent
               | MainPageCardManagerEvent CardManagerEvent CardManagerState
               | MainPageUserAreaEvent    UserAreaEvent    CardManagerState UserAreaState
               | DonationPageEvent        DonationPageEvent

appView :: WidgetState -> Widget HTML PageEvent
appView widgetState@(WidgetState overlayInfo (Tuple page pagesState) proxyInfo)  =
  appPages <> debugState widgetState
  <|>
  overlay overlayInfo

  where 
    appPages :: Widget HTML PageEvent
    appPages = div [Props.className "mainDiv"] [
      headerPage proxyInfo page Loading []
    , SignupPageEvent <$> headerPage proxyInfo page Signup [
        signupFormView pagesState.signup
      ]
    , LoginPageEvent <$> headerPage proxyInfo page Login [
        loginPage (Enabled (page == Login)) pagesState.login
      ]
    , DonationPageEvent <$> headerPage proxyInfo page Donation [
        donationPage pagesState.donation
    ]
    , div [Props.classList (Just <$> ["page", "main", show $ location Main page])] [
        let enableShortcuts = case page of
                                Main -> true
                                _    -> false
            { index
            , userAreaState
            , credentials
            , donationInfo
            , pinExists
            , passkeyExists
            , enableSync
            , cardManagerState
            , userPreferences
            , donationLevel
            , syncDataWire} = pagesState.main
        in
        div [Props._id "homePage"] [
          ( MainPageCardManagerEvent                         # uncurry) <$> cardsManagerView cardManagerState index (unwrap userPreferences).passwordGeneratorSettings donationLevel proxyInfo enableShortcuts         enableSync syncDataWire
        , ((MainPageUserAreaEvent # flip $ cardManagerState) # uncurry) <$> userAreaView     userAreaState userPreferences credentials                                 donationInfo  proxyInfo pinExists passkeyExists enableSync syncDataWire
        ] 
      ]
    ]

data PagePosition = LeftPosition | CenterPosition | RightPosition
instance showPagePosition :: Show PagePosition where
  show LeftPosition   = "left"
  show CenterPosition = "center"
  show RightPosition  = "right"

location :: Page -> Page -> PagePosition
location referencePage currentPage = case referencePage, currentPage of
  Loading , Loading  -> CenterPosition
  Login   , Login    -> CenterPosition
  Signup  , Signup   -> CenterPosition
  Main    , Main     -> CenterPosition
  Donation, Donation -> CenterPosition

  Loading , _        -> LeftPosition
  Signup  , Login    -> LeftPosition
  Login   , Donation -> LeftPosition
  Signup  , Donation -> LeftPosition
  _,        Main     -> LeftPosition
  _,        _        -> RightPosition

pageClassName :: Page -> String
pageClassName Loading  = "loading"
pageClassName Login    = "login"
pageClassName Signup   = "signup"
pageClassName Main     = "main"
pageClassName Donation = "donation"

headerPage :: forall a. ProxyInfo -> Page -> Page -> Array (Widget HTML a) -> Widget HTML a
headerPage proxyInfo currentPage page innerContent = do
  commitHash <- liftEffect $ currentCommit
  div [Props.classList (Just <$> ["page", pageClassName page, show $ location page currentPage])] [
    div [Props.className "content"] [
      proxyInfoComponent proxyInfo []
    , headerComponent
    , div [Props.className "body"] innerContent
    , otherComponent
    , footerComponent commitHash
    ]
  ]

headerComponent :: forall a. Widget HTML a
headerComponent =
  header [] [
    h1 [] [text "clipperz"]
  , h3 [] [text "keep it to yourself"]
  ]

otherComponent :: forall a. Widget HTML a
otherComponent =
  div [(Props.className "other")] [
    div [(Props.className "links")] [
      ul [] [
        li [] [a [Props.href "https://clipperz.is/about/",          Props.target "_blank"] [text "About"]]
      , li [] [a [Props.href "https://clipperz.is/terms_service/",  Props.target "_blank"] [text "Terms of service"]]
      , li [] [a [Props.href "https://clipperz.is/privacy_policy/", Props.target "_blank"] [text "Privacy"]]
      ]
    ]
  ]
