
module Tester where
type TestM a = Either String a

infixr 5 =!=
(=!=) :: (Show a,Eq a) => a -> a -> TestM ()
(=!=) x y = do
  if (x == y)
  then return ()
  else Left $ "Failed assertion " ++ show x ++ " =!= " ++ show y

isLeft :: (Show b) => Either a b -> TestM ()
isLeft (Left _) = return ()
isLeft (Right x) = Left "fail" -- Right (show x) =!= Left "any error message"

runTester :: TestM () -> IO Bool
runTester t =
  case t of
    Left str -> do
      putStrLn str
      return False
    Right () -> do
      return True

