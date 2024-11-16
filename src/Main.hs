module Main where

import Main.Utf8 qualified as Utf8
import Org.Parser

{- |
Main entry point.

`just run` will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    s <- decodeUtf8 <$> readFileBS "/home/srid/org/main.org"
    let doc = parseOrgDoc defaultOrgOptions "main.org" s
    print doc
