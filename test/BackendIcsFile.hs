module BackendIcsFile where

import Tester
import Caligraph.IcsFile.Parser
import Control.Monad
import Data.Ix
import Data.List

testBackendIcsFile = do
  testUnfoldLines

testUnfoldLines :: TestM ()
testUnfoldLines = do
  "foo\n bar" `unfoldsTo` ["foobar"]
  "foobar" `unfoldsTo` ["foobar"]
  "foo\n bar\n  baz" `unfoldsTo` ["foobar baz"]
  where
    unfoldsTo content lines =
        Right lines =!= (fmap (map snd) $ unfoldLines "" content)

