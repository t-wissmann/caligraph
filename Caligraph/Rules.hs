module Caligraph.Rules where

import Caligraph.Config.Types

import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Text.Read

-- | a rule applies of all conditions match, and if this is the case,
-- then all the consequences are executed
data Rule =
  Rule {
    ruleConditions :: [Condition],
    ruleConsequences :: [Consequence]
  }

data Condition
  = CalendarName String
  | DescriptionRegex String

data Consequence
  = ItemVisible Bool
  | ItemColor UiColor
  | ItemColorInv UiColor

parseCondition :: (Text,Text) -> Either String Condition
parseCondition (key,value) = do
  f <- M.lookup (T.unpack key) name2condition `errorName` "no such condition"
  f <$> (readEither $ T.unpack value)
  where
    name2condition = M.fromList
      [ (,) "calendar" CalendarName
      , (,) "description" DescriptionRegex
      ]

errorName :: Maybe a -> String -> Either String a
errorName (Just x) _ = Right x
errorName Nothing s = Left s

-- parseRule :: SectionParser Rule
-- parseRule section =
--   M.toList section
