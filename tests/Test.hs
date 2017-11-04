{-# LANGUAGE DataKinds #-}
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (Handle, SeekMode(..), hSeek, stdout)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import ParamTree

genTestCase :: Int -> Bool -> Char -> Int -> String -> TestTree
genTestCase _ _ _ _ name = testCase name $ return ()

paramTreeTests :: IO ()
paramTreeTests = defaultMain $ testTree genTestCase params
  where
    testTree = growTree (Just "/") testGroup "paramtree"

    params :: Params '[] -> Params [Int, Bool, Char, Int]
    params = simpleParam "Int #1" [1,2]
           . paramSets
                [ simpleParam "Bool" [True] . simpleParam "Char" "xy"
                , simpleParam "Bool" [True,False] . simpleParam "Char" "a"
                ]
           . simpleParam "Int #2" [42,1337]

withCapturedIO :: IO () -> IO BS.ByteString
withCapturedIO act = withSystemTempFile "golden.test" $ \_ hnd -> do
    let redirect :: IO Handle
        redirect = hDuplicate stdout <* hDuplicateTo hnd stdout

        undo :: Handle -> IO ()
        undo h = hDuplicateTo h stdout

    bracket redirect undo $ \_ -> try act :: IO (Either SomeException ())

    hSeek hnd AbsoluteSeek 0
    BS.hGetContents hnd

runGolden :: String -> IO () -> TestTree
runGolden name act = goldenVsString name goldenFile capturedOutput
  where
    goldenFile = "tests/" ++ name ++ ".golden"
    capturedOutput = LBS.fromStrict <$> withCapturedIO act

main :: IO ()
main = defaultMain $ runGolden "paramtree" paramTreeTests
