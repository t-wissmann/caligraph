
module UnicodeJunctionSpec where
import Tester

import Caligraph.Cli.UnicodeJunction

import Data.List (nub)

someLookups :: TestM ()
someLookups = do
  nub junction_types_avail =!= junction_types_avail
  get Empty  Empty Strong Strong =!= '┓'
  get Empty  Empty Strong Normal =!= '┒'
  get Strong Empty Strong Normal =!= '┨'
  where junction_types_avail = map fst characters

