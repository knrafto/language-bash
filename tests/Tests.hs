module Main (main) where

import           Control.Applicative
import           System.Process           (readProcess)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic  as QCM
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Text.Parsec              (parse)

import qualified Language.Bash.Parse      as Parse
import           Language.Bash.Syntax
import qualified Language.Bash.Cond       as Cond
import           Language.Bash.Word
import           Language.Bash.Expand     (braceExpand)
import           Language.Bash.Parse.Word (word)
import           Language.Bash.Word       (unquote)
import qualified Language.Bash.Pretty     as Pretty
-- TODO sequence
braceExpr :: Gen String
braceExpr = concat <$> listOf charset
  where
    charset = oneof $
        [ elements ["{", ",", "}"]
        , elements ["\\ ", "\\{", "\\,", "\\}"]
        , (:[]) <$> elements ['a'..'z']
        ]

expandWithBash :: String -> IO String
expandWithBash str = do
    expn <- readProcess "/usr/bin/env" ["bash", "-c", "echo " ++ str] ""
    return $ filter (`notElem` "\r\n") expn

testExpand :: String -> [String]
testExpand s = case parse word "" s of
    Left  e -> error (show e)
    Right w -> map unquote (braceExpand w)

-- Tests brace expansion.
prop_expandsLikeBash :: Property
prop_expandsLikeBash = monadicIO $ forAllM braceExpr $ \str -> do
    bash <- run $ expandWithBash str
    let check = unwords . filter (not . null) $ testExpand str
    QCM.assert (bash == check)

properties :: TestTree
properties = testGroup "Properties" [testProperty "brace expansion" prop_expandsLikeBash]

testParse name parsed expected = testCase "testTest" $
           case parsed of
               Left err -> assertFailure $ "parseError: " ++ (show err)
               Right ans -> expected @=? ans


testTest = testParse "testTest"
  (Cond.parseTestExpr ["!", "-e", "\"asd\""])
  (Cond.Not (Cond.Unary Cond.FileExists "\"asd\""))

testDoubleQuoted = testParse "testDoubleQuoted"
  (Parse.parse "source" "\"$(ls)\"")
  (List [Statement (Last (Pipeline {timed = False, timedPosix = False, inverted = False, commands = [Command (SimpleCommand [] [[Double [CommandSubst "ls"]]]) []]})) Sequential])


testEmptyArrayAssignment = testParse "testEmptyArrayAssignment"
  (Parse.parse "source" "arguments=()\n")
  (List [Statement (Last (Pipeline {timed = False, timedPosix = False, inverted = False, commands = [Command (SimpleCommand [Assign (Parameter "arguments" Nothing) Equals (RArray [])] []) []]})) Sequential])

unittests :: TestTree
unittests = testGroup "Unit tests" [testTest, testDoubleQuoted, testEmptyArrayAssignment]

tests :: TestTree
tests = testGroup "Tests" [properties, unittests]

main :: IO ()
main = defaultMain tests
