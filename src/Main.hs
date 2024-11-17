{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Lucid
import Main.Utf8 qualified as Utf8
import Org.Parser
import Org.Types
import Web.Scotty qualified as S

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    s <- decodeUtf8 <$> readFileBS "/home/srid/org/main.org"
    let doc = parseOrgDoc defaultOrgOptions "main.org" s
    print $ renderText $ toHtml doc
    S.scotty 4004 $ do
      S.get "/" $ do
        S.html $ renderText $ toHtml doc

instance ToHtml OrgDocument where
  toHtml doc =
    mconcat $ toHtml <$> documentSections doc
  toHtmlRaw = undefined

instance ToHtml OrgSection where
  toHtml (OrgSection {sectionRawTitle, sectionLevel, sectionChildren, sectionSubsections}) =
    div_ $ do
      sectionHeading sectionLevel $ toHtml sectionRawTitle
      div_ [style_ "color: #1a1a1a"] $ do
        forM_ sectionChildren $ \c ->
          toHtml c
      forM_ sectionSubsections $ \s ->
        div_ [style_ "margin-left: 1em; border-left: 1px solid; padding-left: 0.4em"] $ do
          toHtml s
  toHtmlRaw = undefined

instance ToHtml OrgElement where
  toHtml e =
    toHtml $ elementData e
  toHtmlRaw = undefined

instance ToHtml OrgElementData where
  toHtml = \case
    Paragraph p ->
      p_ $
        mapM_ toHtml p
    PlainList {listItems} ->
      ol_ $
        mapM_ toHtml listItems
    x -> code_ $ "Not Implemented for " <> show x
  toHtmlRaw = undefined

instance ToHtml OrgObject where
  toHtml obj =
    case obj of
      Plain s -> toHtml s
      Bold s -> strong_ $ mapM_ toHtml s
      Timestamp ts -> toHtml $ show @Text ts
      x -> code_ $ "Not Implemented for " <> show x
  toHtmlRaw = undefined

instance ToHtml ListItem where
  toHtml (ListItem _ _ _ _objs elems) =
    li_ $ mapM_ toHtml elems
  toHtmlRaw = undefined

sectionHeading :: (Monad m) => Int -> HtmlT m () -> HtmlT m ()
sectionHeading n = h [class_ "section-heading"]
  where
    h = case n of
      1 -> h1_
      2 -> h2_
      3 -> h3_
      4 -> h4_
      5 -> h5_
      6 -> h6_
      _ -> h6_
