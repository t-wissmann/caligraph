
module UnicodeJunctionSpec where

import Caligraph.Cli.UnicodeJunction

import Data.List (nub)

type TestM = Either String ()

infixr 5 =!=
(=!=) :: (Show a,Eq a) => a -> a -> Either String ()
(=!=) x y = do
  if (x == y)
  then return ()
  else Left $ "Failed assertion " ++ show x ++ " =!= " ++ show y

runTester :: TestM -> IO Int
runTester t =
  case t of
    Left str -> do
      error str
      return 1
    Right () -> do
      return 0

someLookups :: TestM
someLookups = do
  nub junction_types_avail =!= junction_types_avail
  get Empty  Empty Strong Strong =!= '┓'
  get Empty  Empty Strong Normal =!= '┒'
  get Strong Empty Strong Normal =!= '┨'
  where junction_types_avail = map fst characters

