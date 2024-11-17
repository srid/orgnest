{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Map.Strict qualified as Map
import Lucid
import Main.Utf8 qualified as Utf8
import Org.Parser
import Org.Types
import Web.Scotty qualified as S

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    do
      s <- decodeUtf8 <$> readFileBS "/home/srid/org/main.org"
      let doc = parseOrgDoc defaultOrgOptions "main.org" s
      print $ renderText $ toHtml doc
    S.scotty 4004 $ do
      S.get "/" $ do
        s <- decodeUtf8 <$> readFileBS "/home/srid/org/main.org"
        let doc = parseOrgDoc defaultOrgOptions "main.org" s
        S.html $ renderText $ layout $ toHtml doc

instance ToHtml OrgDocument where
  toHtml doc =
    mconcat $ toHtml <$> documentSections doc
  toHtmlRaw = undefined

instance ToHtml OrgSection where
  toHtml (OrgSection {sectionTitle, sectionLevel, sectionProperties, sectionChildren, sectionSubsections}) = do
    let attrs = case Map.lookup "id" sectionProperties of
          Just _id -> [id_ _id]
          Nothing -> []
    div_ attrs $ do
      sectionHeading sectionLevel $ mapM_ toHtml sectionTitle
      div_ [style_ "margin-left: 0.7em; padding-left: 0.4em"] $ do
        div_ [style_ ""] $ do
          forM_ sectionChildren $ \c ->
            toHtml c
        forM_ sectionSubsections $ \s ->
          toHtml s
  toHtmlRaw = undefined

instance ToHtml OrgElement where
  toHtml e =
    toHtml $ elementData e
  toHtmlRaw = undefined

instance ToHtml OrgElementData where
  toHtml = \case
    Paragraph p ->
      p_ [style_ "margin-top: 1em; margin-bottom: 1em; "] $
        mapM_ toHtml p
    PlainList {listType, listItems} -> do
      let genList = case listType of
            Ordered _ -> ol_
            _ -> ul_
      genList $
        forM_ listItems $ \(ListItem _ _ _ _ items) ->
          li_ $ case elementData <$> items of
            -- Strip paragraphs in list outlines
            [Paragraph p] -> mapM_ toHtml p
            [Paragraph p, x@(PlainList _ _)] -> do
              mapM_ toHtml p
              toHtml x
            x -> todo "LIST" $ show x -- need to handle other cases
    GreaterBlock {blkElements} -> do
      blockquote_ [style_ "border-left: 1px solid; padding-left: 1em; color: darkslategray;"] $ do
        mapM_ toHtml blkElements
    x -> todo "OrgElementData" $ show x
  toHtmlRaw = undefined

instance ToHtml OrgObject where
  toHtml obj =
    case obj of
      Plain s -> toHtml s
      Bold s -> strong_ $ mapM_ toHtml s
      Italic s -> em_ $ mapM_ toHtml s
      Underline s -> span_ [style_ "text-decoration: underline"] $ mapM_ toHtml s
      Verbatim s -> code_ [style_ "font-size: 0.7em;"] $ toHtml s
      Timestamp ts -> todo "Timestamp" $ show @Text ts
      Link url x -> a_ [href_ $ linkTargetToText url] $ mapM_ toHtml x
      Quoted q x -> do
        let qs = case q of
              SingleQuote -> "'"
              DoubleQuote -> "\""
        qs
        mapM_ toHtml x
        qs
      x -> code_ $ "TODO(OrgObject) " <> show x
  toHtmlRaw = undefined

todo :: (Monad m) => Text -> Text -> HtmlT m ()
todo t s = do
  span_ [style_ "color: #ff0000"] $ do
    toHtml $ "TODO(" <> t <> ")"
  code_ $ toHtml s

instance ToHtml ListItem where
  toHtml (ListItem _ _ _ objs elems) =
    li_ $ do
      mapM_ (todo "ListItem:objs[]" . show) objs
      mapM_ toHtml elems
  toHtmlRaw = undefined

sectionHeading :: (Monad m) => Int -> HtmlT m () -> HtmlT m ()
sectionHeading n = h
  where
    h = case n of
      1 -> h1_ [style_ $ baseStyle <> "font-size: " <> size 1.5 <> ";"]
      2 -> h2_ [style_ $ baseStyle <> "font-size: " <> size 1.4 <> "; background-color: bisque; font-weight: bold; padding: 0.1em 0.3em; border-radius: 0.1em;"]
      3 -> h3_ [style_ $ baseStyle <> "font-size: " <> size 1.3 <> "; background-color: lightyellow; font-weight: bold; padding: 0.1em 0.3em; border-radius: 0.1em;"]
      4 -> h4_ [style_ $ baseStyle <> "font-size: " <> size 1.2 <> ";"]
      5 -> h5_ [style_ $ baseStyle <> "font-size: " <> size 1.15 <> ";"]
      _ -> h6_ [style_ $ baseStyle <> "font-size: " <> size 1.10 <> ";"]
    baseStyle = "font-weight: normal; margin: 0.5em 0 0.5em; color: blueviolet;"
    size (x :: Double) = show x <> "em"

layout :: Html () -> Html ()
layout content = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8", name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "Orgnest"
      -- reset css
      script_ [src_ "https://unpkg.com/htmx.org@2.0.3", integrity_ "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq", crossorigin_ "anonymous"] $ fromString @Text ""
      link_ [rel_ "stylesheet", type_ "text/css", href_ "https://unpkg.com/modern-css-reset/dist/reset.min.css"]
      style_ "@import url('https://fonts.googleapis.com/css2?family=DM+Serif+Display:ital@0;1&family=EB+Garamond:ital,wght@0,400..800;1,400..800&family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap');"
    body_ $ do
      div_
        [style_ "font-size: 18px; margin-left: 1em"]
        content
