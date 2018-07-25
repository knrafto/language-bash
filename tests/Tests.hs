module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           System.Process           (readProcess)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic  as QCM
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Test.Tasty.ExpectedFailure (expectFail)
import           Text.Parsec              (parse)
import           Text.Parsec.Error (ParseError)


import qualified Language.Bash.Parse      as Parse
import           Language.Bash.Syntax
import qualified Language.Bash.Cond       as Cond
import           Language.Bash.Word
import           Language.Bash.Expand     (braceExpand)
import           Language.Bash.Parse.Word (word)

-- TODO sequence
braceExpr :: Gen String
braceExpr = concat <$> listOf charset
  where
    charset = oneof
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
    when (bash /= check) $ do
      run $ putStrLn "FAIL"
      run $ putStrLn "input:"
      run $ putStrLn str
      run $ putStrLn "bash output:"
      run $ putStrLn bash
      run $ putStrLn "test output:"
      run $ putStrLn check
      QCM.assert False

properties :: TestTree
properties = testGroup "Properties" [testProperty "brace expansion" prop_expandsLikeBash]

testMatches :: (Eq a, Show a) => TestName -> Either Text.Parsec.Error.ParseError a -> a -> TestTree
testMatches name parsed expected = testCase name $
           case parsed of
               Left err -> assertFailure $ "parseError: " ++ show err
               Right ans -> expected @=? ans

wrapCommand :: Command -> List
wrapCommand c = List [Statement (Last Pipeline {timed = False, timedPosix = False, inverted = False, commands = [c]}) Sequential]

tp :: TestName -> Command -> TestTree
tp source expected = testMatches (filter ((/=) '\n') source)
                                 (Parse.parse "source" source)
                                 (wrapCommand expected)

expandString :: String -> [Span]
expandString = map Char

unittests :: TestTree
unittests = testGroup "Unit tests"
  [
    testMatches "testTest"
      (Cond.parseTestExpr ["!", "-e", "\"asd\""])
      (Cond.Not (Cond.Unary Cond.FileExists "\"asd\""))
  , tp "\"$(ls)\""
      (Command (SimpleCommand [] [[Double [CommandSubst "ls"]]]) [])
  , tp "arguments=()"
      (Command (SimpleCommand [Assign (Parameter "arguments" Nothing) Equals (RArray [])] []) [])
  , tp "cat <<EOF\nasd\\`\nEOF"
       (Command
        (SimpleCommand [] [expandString "cat"])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF",
                  heredocDelimQuoted = False,
                  hereDocument = [Char 'a',Char 's',Char 'd',Escape '`',
                                  Char '\n']}])
  , tp "cat <<\"EOF\"\nasd\\`\nEOF"
       (Command
        (SimpleCommand [] [expandString "cat"])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF",
                  heredocDelimQuoted = True,
                  hereDocument = expandString "asd\\`\n"}])
  , tp "echo $((2 + 2))"
       (Command
        (SimpleCommand [] [expandString "echo", [ArithSubst "2 + 2"]])
        [])
  , tp "((2 + 2))"
       (Command (Arith "2 + 2") [])
  , tp "echo $(((2 + 2)))"
       (Command
        (SimpleCommand [] [expandString "echo", [ArithSubst "(2 + 2)"]])
        [])
  , tp "function-name-with-dashes() { :; }"
       (Command
        (FunctionDef "function-name-with-dashes"
          (List [Statement (Last (Pipeline {timed = False, timedPosix = False, inverted = False, commands =
            [Command (SimpleCommand [] [[Char ':']]) []]})) Sequential])) [])
  ]

failingtests :: TestTree
failingtests = testGroup "Failing tests" (map expectFail
  [])

tests :: TestTree
tests = testGroup "Tests" [properties, unittests, failingtests]

main :: IO ()
main = defaultMain tests
