module DataModel.AppError where

import Data.Eq (class Eq)
import Data.PrettyShow (class PrettyShow, prettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Communication.ProtocolError (ProtocolError)

type Element = String
type WrongVersion = String

data InvalidStateError = CorruptedState String | MissingValue String | CorruptedSavedPassphrase String
instance showInvalidStateError :: Show InvalidStateError where
  show (CorruptedState           s) = "Corrupted state: " <> s
  show (MissingValue             s) = "Missing value in state: " <> s
  show (CorruptedSavedPassphrase s) = "Corrupted passphrase in local storage: " <> s

derive instance eqInvalidStateError :: Eq InvalidStateError

instance prettyShowInvalidStateError :: PrettyShow InvalidStateError where
  prettyShow (CorruptedState           _) = "The application state is corrupted, please restart it."
  prettyShow (MissingValue             _) = "The application state is corrupted, please restart it."
  prettyShow (CorruptedSavedPassphrase _) = "Clipperz could not decrypt your credentials, please log in without using the device PIN."

data AppError = InvalidStateError InvalidStateError | ProtocolError ProtocolError | ImportError String | CannotInitState String | InvalidOperationError String | InvalidVersioning Element WrongVersion | OperationUnsupported | OperationCanceled | UnhandledCondition String
instance showAppError :: Show AppError where
  show (InvalidStateError err)      = "Invalid state Error: "  <> show err
  show (ProtocolError err)          = "Protocol Error: " <> show err
  show (ImportError err)            = "Import Error: " <> err
  show (CannotInitState err)        = "Cannot init state: " <> err
  show (InvalidOperationError err)  = "Invalid operation error: " <> err
  show (InvalidVersioning elem v)   = "Invalid Versioning [" <> v <> "] of " <> elem 
  show (OperationUnsupported)       = "Operation unsupported"
  show (OperationCanceled)          = "Operation canceled"
  show (UnhandledCondition err)     = "Unahandled Condition: " <> err

instance prettyShowAppError :: PrettyShow AppError where
  prettyShow (InvalidStateError err)    = prettyShow err
  prettyShow (ProtocolError err)        = prettyShow err
  prettyShow (ImportError err)          = "Your imported values are not in the right format! (" <> err <> ")" 
  prettyShow (CannotInitState _)        = "Cannot init state, please try to reload"
  prettyShow (InvalidOperationError _)  = "Invalid operation error, something was not programmed correctly."
  prettyShow (InvalidVersioning elem _) = elem <> " is encoded in an unsupported version" 
  prettyShow (OperationUnsupported)     = "Unsupported"
  prettyShow (OperationCanceled)        = "Canceled"
  prettyShow (UnhandledCondition     _) = ""

derive instance eqAppError :: Eq AppError
