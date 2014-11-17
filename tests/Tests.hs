module Main where

import Control.Applicative
import System.Process (readProcess)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Parsec (parse)

import Language.Bash.Expand (braceExpand)
import Language.Bash.Parse.Word (word)
import Language.Bash.Word (unquote)

testExpand :: String -> [String]
testExpand s = case parse word "" s of
    Left  e -> error (show e)
    Right w -> map unquote (braceExpand w)

charset :: Gen String
charset = elements $
    ["", ",", "\\{", "\\}", "\\,", "\\ "] ++
    map (:[]) ['a'..'z']

junk :: Int -> Gen String
junk sz = concat <$> resize sz (listOf charset)

braced :: Gen String
braced = do
    s <- junk 10
    frequency
        [ (5, braced s)
        , (1, llbraced s)
        , (1, lrbraced s)
        , (1, rlbraced s)
        , (1, rrbraced s)
        , (1, str_only s)
        ]
  where
    braced s   = return $ "{" ++ s ++ "}"
    llbraced s = return $ "{" ++ s
    lrbraced s = return $ "}" ++ s
    rlbraced s = return $        s ++ "{"
    rrbraced s = return $        s ++ "}"
    str_only   = return

braceExpr :: Gen String
braceExpr = sized go
  where
    go 0 = braced
    go n = (++) <$> go (n `div` 2) <*> go (n `div` 2)

expandWithBash :: String -> IO String
expandWithBash str = do
    expn <- readProcess "/bin/bash" ["-c", "echo " ++ str] ""
    return $ filter (`notElem` "\r\n") expn

-- Tests brace expansion.
-- TODO: sequence expansions
prop_expandsLikeBash :: Property
prop_expandsLikeBash = monadicIO $ forAllM braceExpr $ \str -> do
    bash <- run $ expandWithBash str
    let check = unwords . filter (not . null) $ testExpand str
    run . putStrLn . unlines $
        [ "On: " ++ str
        , "  Bash: " ++ bash
        , "  This: " ++ check
        ]
    assert $ bash == check

tests :: TestTree
tests = testProperty "brace expansion" prop_expandsLikeBash

main :: IO ()
main = defaultMain tests
