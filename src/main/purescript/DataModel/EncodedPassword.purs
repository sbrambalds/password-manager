module DataModel.EncodedPassword where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type EncodedPassword = { padding :: Int, passphrase :: String }
encodedPasswordCodec :: CA.JsonCodec EncodedPassword
encodedPasswordCodec =
  CAR.object "encodedPassword" { padding : CA.int , passphrase : CA.string }
