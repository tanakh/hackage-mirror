module Haddock (
  format,
  ) where

import Import
import Shelly
import GHC
import DynFlags
import Outputable

import Data.Maybe
import qualified Data.Text.Lazy as LT
import Distribution.Verbosity
import Documentation.Haddock
import qualified GHC.Paths as GhcPaths

format :: LT.Text -> IO Html
format txt = shelly $ withTmpDir $ \dir -> chdir dir $ do
  writefile "tmp.hs" $ LT.unlines $ "{- | " : LT.lines txt ++ ["-}", "module Main where", "main = undefined"]
  apath <- absPath "tmp.hs"
  ([Interface {..}], _) <- liftIO $ do
    runGhc (Just GhcPaths.libdir) $ do
      flg <- getSessionDynFlags 
      setSessionDynFlags $ dopt_set flg Opt_Haddock
      processModules verbose [LT.unpack $ toTextIgnore apath] [] []
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
  markupCodeBlock h = [hamlet|<pre.prettyprint.linenum>^{h}|]
  
  markupUnorderedList hs = [hamlet|<ul>^{mconcat $ map f hs}|] where
    f h = [hamlet|<li>^{h}|]
  markupOrderedList hs = [hamlet|<pl>^{mconcat $ map f hs}|] where
    f h = [hamlet|<li>^{h}|]
  
  markupExample = mconcat . map f where
    f Example {..} = [hamlet|<pre>#{unlines ss}|] where
      ss = ["> " ++ exampleExpression] ++ exampleResult
 
  -- not tested
  markupIdentifierUnchecked (modName, occName) = [hamlet|#{showPpr occName}|]
  markupModule m = [hamlet|<code>#{m}|]
  markupEmphasis h = [hamlet|<emph>^{h}|]
  markupAName h = [hamlet|<code>#{h}|]
  markupPic url = [hamlet|<img src=#{url}>|]
  
  markupDefList defs = [hamlet|<dl>^{mconcat $ map f defs}|] where
    f (term, def) = [hamlet|
<dt>^{term}
<dd>^{def}
|]
