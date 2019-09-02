module Caligraph.Rules where

import Graphics.Vty.Attributes

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
  | ItemColor (Maybe Color)
  | ItemColorInv (Maybe Color)
