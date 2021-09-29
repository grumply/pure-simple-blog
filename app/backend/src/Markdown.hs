module Markdown where

import Shared (Markdown(..))
import Pure.Elm.Component (View)

import Pure.TagSoup (parseView)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Options (def,ReaderOptions(..),pandocExtensions)

process :: Markdown -> [View]
process (Markdown md) = either (const []) id $ runPure $ do
  one <- readMarkdown def { readerExtensions = pandocExtensions } md
  two <- writeHtml5String def one
  pure (parseView two)