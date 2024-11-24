module Views.LoginFormView
  ( LoginPageEvent(..)
  , PinCredentials
  , Username
  , credentialLoginWidget
  , emptyLoginFormData
  , isFormValid
  , loginPage
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, input, label, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>), (<$), (<|>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Eq ((/=), (==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), not)
import Data.Ord ((<))
import Data.String (length)
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.WidgetState (LoginFormData, LoginType(..))
import Effect.Class (liftEffect)
import Functions.Communication.OneTimeShare (PIN)
import Functions.Events (focus)

type Username = String

type PinCredentials = { pin :: Int, user :: Username, passphrase :: String }

data LoginPageEvent   = LoginEvent Credentials
                      | LoginPinEvent PIN
                      | GoToCredentialLoginEvent Username
                      | GoToSignupEvent Credentials
                      | UpdateForm LoginFormData

emptyLoginFormData :: LoginFormData
emptyLoginFormData = { credentials: emptyCredentials, pin: "", loginType: CredentialLogin }

isFormValid :: Credentials -> Boolean
isFormValid { username, password } = username /= "" && password /= ""


loginPage :: LoginFormData -> Widget HTML LoginPageEvent
loginPage formData@{credentials, pin, loginType} =
  case loginType of
    CredentialLogin -> credentialLoginWidget formData
    PinLogin        -> do
      form [Props.className "loginForm"] [
        div [Props.className "loginInputs"] [
          span [] [text "Enter your PIN"]
        , pinLoginWidget (length pin < 5) formData
        , GoToCredentialLoginEvent credentials.username <$ a [Props.onClick] [text "Login with passphrase"]
        ]
      ]

credentialLoginWidget :: LoginFormData -> Widget HTML LoginPageEvent
credentialLoginWidget formData@{credentials: credentials@{username, password}} = do
    form [Props.className "loginForm"] [
      div [Props.className "loginInputs"] [
        label [] [
            span [Props.className "label"] [text "Username"]
          , (Props.unsafeTargetValue) <$> input [
              Props._type "text"
            , Props._id "loginUsernameInput"
            , Props.placeholder "username"
            , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
            , Props.value username
            , Props.autoFocus true
            , Props.disabled false
            , Props.onChange
            ]
          ] <#> (\username' -> UpdateForm formData {credentials = credentials {username = username'}})
      , label [] [
          span [Props.className "label"] [text "Passphrase"]
        , (Props.unsafeTargetValue) <$> input [
            Props._type "password"
          , Props._id "loginPassphraseInput"
          , Props.placeholder "passphrase"
          , Props.value password
          , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
          , Props.disabled false
          , Props.onChange
          ]
        ] <#> (\password' -> UpdateForm formData {credentials = credentials {password = password'}})
      ]
  , ((div [Props.className "loginButton"] [
        button  [ Props.onClick $> (LoginEvent credentials)
                , Props.className "login"
                , Props.disabled (not (isFormValid credentials))
                ] [span [] [text "login"]]
      ])
      <|> 
      (button [Props.onClick] [
        text "sign up"
      ] *> (focus "signupUsernameInput" # liftEffect) $> (GoToSignupEvent credentials)
      )
    )
  ]

pinLoginWidget :: Boolean -> LoginFormData -> Widget HTML LoginPageEvent
pinLoginWidget active loginFormData@{pin} = do
  pin' <- div [] [
    label [] [
      span [Props.className "label"] [text "Login"]
    , (Props.unsafeTargetValue) <$> input [
        Props._type "tel"
      , Props._id "loginPINInput"
      , Props.placeholder "PIN"
      , Props.value pin
      , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
      , Props.autoFocus active
      , Props.disabled (not active)
      , Props.onChange
      , Props.pattern "[0-9]{5}"
      ]
    ]
  ]
  pure $ if (length pin' == 5) && active then LoginPinEvent pin' else UpdateForm loginFormData {pin = pin'}
