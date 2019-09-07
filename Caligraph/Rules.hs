module Caligraph.Rules where

import Caligraph.Config.Types
import Caligraph.Config.Main (getSource)

import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Text.Read
import Data.Either (partitionEithers)
import Data.List (intersperse)
import Control.Monad (unless)
import System.Environment.XDG.BaseDir

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

parseConditionConsequence :: (Text,Text) -> Either String (Either Condition Consequence)
parseConditionConsequence (key,value) = do
  f <- M.lookup (T.unpack key) name2conditionOrConsequence
        `errorName` ("no such condition or consequence: " ++ T.unpack key)
  f (T.unpack value)
  where
    name2conditionOrConsequence :: M.HashMap String (String -> Either String (Either Condition Consequence))
    name2conditionOrConsequence = M.fromList $
      mapSnds (\constr -> Right . Left . constr) name2condition
      ++ mapSnds ((.) $ fmap Right) name2consequence

    mapSnds :: (a -> b) -> [(x,a)] -> [(x,b)]
    mapSnds f = map (\(x,a) -> (x, f a))

    name2condition :: [(String,String -> Condition)]
    name2condition =
      [ (,) "calendar" CalendarName
      , (,) "description" DescriptionRegex
      ]
    name2consequence :: [(String,String -> Either String Consequence)]
    name2consequence =
      [ c "visible"   ItemVisible
      , c "color"     ItemColor
      , c "color-inv" ItemColorInv
      ]
    c :: UserReadShow x => String -> (x -> Consequence) -> (String, String -> Either String Consequence)
    c name constructor = (name, \s -> constructor <$> userRead s)

errorName :: Maybe a -> String -> Either String a
errorName (Just x) _ = Right x
errorName Nothing s = Left s

parseRule :: SectionParser Rule
parseRule section = do
  let (ls,rs) = partitionEithers $ map parseConditionConsequence (M.toList section)
  unless (null ls) $ -- if ls is not empty
    -- then fail with all error messages present
    Left $ concat $ intersperse ['\n'] ls
  let (condits,conseqs) = partitionEithers rs
  return (Rule condits conseqs)

