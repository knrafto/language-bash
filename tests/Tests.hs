module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           System.Directory (listDirectory)
import           System.FilePath ((</>), (-<.>), takeExtension)
import           System.Process           (readProcess)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic  as QCM
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Test.Tasty.ExpectedFailure (expectFail)
import           Text.Parsec              (parse)
import           Text.Parsec.Error (ParseError)


import qualified Language.Bash.Parse      as Parse
import           Language.Bash.Pretty     (prettyText)
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
properties = testGroup "Properties"
    -- TODO: re-enable once #17 is fixed.
    [ -- testProperty "brace expansion" prop_expandsLikeBash
    ]

discoverPrettyTests :: FilePath -> IO TestTree
discoverPrettyTests fp = do
    fs <- listDirectory fp
    let shFs = filter (\fp' -> takeExtension fp' == ".sh") fs
    return $ testGroup "Pretty printing" $ map (\shFn -> testPretty shFn (fp </> shFn)) shFs

testPretty :: TestName -> FilePath -> TestTree
testPretty name shFp = do
    let outFp    = shFp -<.> "out"
        goldenFp = shFp -<.> "golden"
    goldenVsFileDiff name (\ref new -> ["diff", "-u", ref, new]) goldenFp outFp $ do
        cnt <- readFile shFp
        parsed <- case Parse.parse shFp cnt of
            Left err -> assertFailure $ show err
            Right parsed -> return parsed
        writeFile outFp $ prettyText parsed ++ "\n"

testMatches :: (Eq a, Show a) => TestName -> Either Text.Parsec.Error.ParseError a -> a -> TestTree
testMatches name parsed expected = testCase name $
           case parsed of
               Left err -> assertFailure $ "parseError: " ++ show err
               Right ans -> expected @=? ans

wrapCommands :: [Command] -> List
wrapCommands = List . map (\c -> Statement (Last Pipeline {timed = False, timedPosix = False, inverted = False, commands = [c]}) Sequential)

wrapCommand :: Command -> List
wrapCommand c = wrapCommands [c]

tp :: TestName -> List -> TestTree
tp source expected = testMatches (show source)
                                 (Parse.parse "source" source)
                                 expected

unittests :: TestTree
unittests = testGroup "Unit tests"
  [
    testMatches "testTest"
      (Cond.parseTestExpr ["!", "-e", "\"asd\""])
      (Cond.Not (Cond.Unary Cond.FileExists "\"asd\""))
  , tp "\"$(ls)\"" $ wrapCommand
      (Command (SimpleCommand [] [[Double [CommandSubst "ls"]]]) [])
  , tp "arguments=()" $ wrapCommand
      (Command (SimpleCommand [Assign (Parameter "arguments" Nothing) Equals (RArray [])] []) [])
  , tp "cat <<EOF\nasd\\`\nEOF" $ wrapCommand
       (Command
        (SimpleCommand [] [stringToWord "cat"])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF",
                  heredocDelimQuoted = False,
                  hereDocument = [Char 'a',Char 's',Char 'd',Escape '`',
                                  Char '\n']}])
  , tp "cat <<EOF\nfoo\nEOF\n  cat" $ wrapCommands
      [ Command
          (SimpleCommand [] [stringToWord "cat"])
          [Heredoc {heredocOp = Here,
                    heredocDelim = "EOF",
                    heredocDelimQuoted = False,
                    hereDocument = [Char 'f',Char 'o',Char 'o',Char '\n']}]
      , Command (SimpleCommand [] [stringToWord "cat"]) []
      ]
  , tp "cat <<\"EOF\"\nasd\\`\nEOF" $ wrapCommand
       (Command
        (SimpleCommand [] [stringToWord "cat"])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF",
                  heredocDelimQuoted = True,
                  hereDocument = stringToWord "asd\\`\n"}])
  , tp "cat <<EOF1 <<EOF2\nt1\nEOF1\nt2\nEOF2" $ wrapCommand
        (Command
        (SimpleCommand [] [stringToWord "cat"])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF1",
                  heredocDelimQuoted = False,
                  hereDocument = stringToWord "t1\n"}
        ,Heredoc {heredocOp = Here,
                  heredocDelim = "EOF2",
                  heredocDelimQuoted = False,
                  hereDocument = stringToWord "t2\n"}])
  , tp "echo $((2 + 2))" $ wrapCommand
       (Command
        (SimpleCommand [] [stringToWord "echo", [ArithSubst "2 + 2"]])
        [])
  , tp "((2 + 2))" $ wrapCommand
       (Command (Arith "2 + 2") [])
  , tp "echo $(((2 + 2)))" $ wrapCommand
       (Command
        (SimpleCommand [] [stringToWord "echo", [ArithSubst "(2 + 2)"]])
        [])
  , tp "function foo() { true; }" $ wrapCommand
       (Command
        (FunctionDef "foo"
          (wrapCommand (Command (SimpleCommand [] [stringToWord "true"]) []))) [])
  , tp "!awesome-function-name-2.0+() { true; }" $ wrapCommand
       (Command
        (FunctionDef "!awesome-function-name-2.0+"
          (wrapCommand (Command (SimpleCommand [] [stringToWord "true"]) []))) [])
  , tp "cat <<EOF\nasd\nEOF\ntrue" $ wrapCommands
       [Command
        (SimpleCommand [] [stringToWord "cat"])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF",
                  heredocDelimQuoted = False,
                  hereDocument = stringToWord "asd\n"}],
        Command (SimpleCommand [] [stringToWord "true"]) []]
  , tp "true\r\nfalse" $ wrapCommands
       [Command (SimpleCommand [] [stringToWord "true"]) [],
        Command (SimpleCommand [] [stringToWord "false"]) []]
  , tp "[[ ! ( -f foo ) ]]" $ wrapCommand
      (Command (Cond (Cond.Not (Cond.Unary Cond.RegularFile (stringToWord "foo")))) [])
  , tp "[[ foo =~ a( b )c\" d\\\"\"[e]*' f '\\ g ]]" $ wrapCommand
      (Command (Cond (Cond.Binary (stringToWord "foo") Cond.StrMatch (stringToWord "a( b )c\" d\\\"\"[e]*' f '\\ g"))) [])
  , tp "for function in foo; do true; done" $ wrapCommand
       (Command
        (For "function" (WordList [stringToWord "foo"])
          (wrapCommand (Command (SimpleCommand [] [stringToWord "true"]) []))) [])
  , tp "<<EOF\ncomment\nEOF" $ wrapCommand
       (Command
        (SimpleCommand [] [])
        [Heredoc {heredocOp = Here,
                  heredocDelim = "EOF",
                  heredocDelimQuoted = False,
                  hereDocument = stringToWord "comment\n"}])
  , testGroup "Issue #23 regression tests"
    [ testCase "Empty document" $ do
          let list = wrapCommand $ Command
                  (SimpleCommand [] [stringToWord "command"])
                  [ Heredoc
                      { heredocOp = Here
                      , heredocDelim = "EOF"
                      , heredocDelimQuoted = False
                      , hereDocument = []
                      }
                  ]
              expected = "command <<EOF;\nEOF"
          expected @=? prettyText list
    , testCase "Non-empty document" $ do
          let list = wrapCommand $ Command
                  (SimpleCommand [] [stringToWord "command"])
                  [ Heredoc
                      { heredocOp = Here
                      , heredocDelim = "EOF"
                      , heredocDelimQuoted = False
                      , hereDocument = stringToWord "here document"
                      }
                  ]
              expected = "command <<EOF;\nhere document\nEOF"
          expected @=? prettyText list
    ]
  ]

failingtests :: TestTree
failingtests = testGroup "Failing tests" (map expectFail
  [])

main :: IO ()
main = do
    pptests <- discoverPrettyTests "tests/pretty"
    defaultMain $ testGroup "Tests"
        [ properties
        , unittests
        , failingtests
        , pptests
        ]
