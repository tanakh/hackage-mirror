module Haddock (
  format,
  ) where

import Import
import Shelly
import GHC
import Outputable

import Data.Maybe
import qualified Data.Text.Lazy as LT
import Documentation.Haddock

format :: LT.Text -> IO Html
format txt = shelly $ withTmpDir $ \dir -> chdir dir $ do
  writefile "main.hs" $ LT.unlines $ "module Main where" : (map ("--| " `mappend`) $ LT.lines txt)
  [Interface {..}] <- liftIO $ createInterfaces [] ["main.hs"]
  return $ markup mkup (fromJust ifaceDoc) ""

mkup :: DocMarkup Name (t -> Html)
mkup = Markup {..} where
  markupEmpty = [hamlet||]
  markupAppend = mappend
  markupString s = [hamlet|#{s}|]
  markupParagraph a = [hamlet|<p>^{a}|]
  markupMonospaced h = [hamlet|<code>^{h}|]
  markupURL url = [hamlet|<a href=#{url}>#{url}|]
  markupIdentifier i = [hamlet|#{showPpr i}|]
  markupCodeBlock h = [hamlet|<pre>^{h}|]
  
  markupUnorderedList hs = [hamlet|<ul>^{mconcat $ map f hs}|] where
    f h = [hamlet|<li>^{h}|]
  markupOrderedList hs = [hamlet|<pl>^{mconcat $ map f hs}|] where
    f h = [hamlet|<li>^{h}|]
  
  markupExample = mconcat . map f where
    f Example {..} = [hamlet|<pre>#{unlines ss}|] where
      ss = ["> " ++ exampleExpression] ++ exampleResult
 
  -- not tested
  markupIdentifierUnchecked (modName, occName) = [hamlet|#{showPpr occName}|]
  markupModule m = [hamlet|<h1>#{m}|]
  markupEmphasis h = [hamlet|<emph>^{h}|]
  markupAName h = [hamlet|<code>#{h}|]
  markupPic url = [hamlet|<img src=#{url}>|]
  
  markupDefList defs = [hamlet|<dl>^{mconcat $ map f defs}|] where
    f (term, def) = [hamlet|
<dt>^{term}
<dd>^{def}
|]
