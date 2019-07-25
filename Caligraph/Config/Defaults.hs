{-# LANGUAGE TemplateHaskell #-}
module Caligraph.Config.Defaults where

import Data.String
import Data.Text
import Data.FileEmbed

defaultKeys :: Text
defaultKeys = pack "" -- $(embedStringFile "example-config/keys.ini")
