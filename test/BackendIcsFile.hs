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
  "foo\n\tbar" `unfoldsTo` ["foobar"]
  "foo\n bar\n\t baz\nextra" `unfoldsTo` ["foobar baz", "extra"]
  "a" `unfoldsTo` ["a"]
  "a\n" `unfoldsTo` ["a"]
  "a\nb\nc\n" `unfoldsTo` ["a","b","c"]
  "a\nb\nc " `unfoldsTo` ["a","b","c "]
  "a\nb\n c\nd" `unfoldsTo` ["a","bc","d"]
  where
    unfoldsTo content lines =
        Right lines =!= (fmap (map snd) $ unfoldLines "" content)

