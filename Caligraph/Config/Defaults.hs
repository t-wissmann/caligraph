{-# LANGUAGE TemplateHaskell #-}
module Caligraph.Config.Defaults where

import Data.String
import Data.Text
import Data.FileEmbed

defaultKeys :: Text
defaultKeys = $(embedStringFile "example-config/keys.ini")
