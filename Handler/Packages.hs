{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Handler.Packages where

import Import
import Yesod.Paginator

import Control.Monad
import Control.Monad.Trans
import Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Filesystem.Path as FP
import Text.Shakespeare.Text
import Text.Regex.TDFA

import Shelly as S

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

import Text.Pandoc

appDir :: String
appDir = "/home/tanakh/.hackage"

getPackagesR :: Handler RepHtml
getPackagesR = do
  (T.head . fromMaybe "A" -> curL) <- lookupGetParam "letter"
  (read . T.unpack . fromMaybe "1" -> page) <- lookupGetParam "p"

  let (cur, suc) =
        (T.singleton curL, T.singleton (succ curL))
  
  let letters = map (\c->[c]) ['A'..'Z']

  pkgs <- runDB $ do
    pkgs <- map entityVal <$>
      selectList ( [ PackageDownloaded ==. True ] ++
                   ( [PackageName >=. T.toLower cur, PackageName <. T.toLower suc] ||.
                     [PackageName >=. T.toUpper cur, PackageName <. T.toUpper suc]
                   )
                 )
                 [ Asc PackageName, Asc PackageVersion ]
    return . map last . groupBy (\a b -> packageName a == packageName b) $ pkgs
  
  let total = length pkgs
      ppp = 25
      from = (page - 1) * ppp + 1
      to   = min total $ from + ppp
  
  (pkgs', paginationWidget) <- paginate ppp pkgs

  defaultLayout $ do
    setTitle "HackageDB mirror - Packages"
    $(widgetFile "packages")

getPackageInfoR :: Text -> Handler RepHtml
getPackageInfoR pkgFull = do
  [[_, _, pkgName', pkgVersion', _, pkgNoVer']] <- T.unpack pkgFull =~~ ("((.+)-([0-9]+(\\.[0-9]+)*))|(.+)$" :: String)
  let (pkgName, pkgVersion)
        | not $ null pkgNoVer' =
          (T.pack pkgNoVer', Nothing)
        | otherwise =
          (T.pack pkgName', Just $ T.pack pkgVersion')
  
  pkg <- runDB $ selectList [PackageName ==. pkgName] [Desc PackageVersion]
  Entity _ pkg <- maybe notFound return $
                  L.find (\(Entity _ val) -> case pkgVersion of
                             Nothing -> True
                             Just version -> packageVersion val == version) pkg
  
  counts <- runDB $ do
    vers <- selectList [PackageName ==. pkgName] []
    forM vers $ \(Entity key val) -> do
      (packageVersion val, ) <$> count [DownloadPackage ==. key]

  let mycnt = fromMaybe 0 $ lookup (packageVersion pkg) counts
      totcnt = sum $ map snd counts
      arcName = packageName pkg <> "-" <> packageVersion pkg <> ".tar.gz"

  cabal <- getCabal (packageName pkg) (packageVersion pkg)
  GenericPackageDescription {..} <- case parsePackageDescription $ T.unpack cabal of
    ParseFailed _ -> fail "cabal parse error"
    ParseOk _ desc -> return desc

  let PackageDescription {..} = packageDescription

  let doc = readMarkdown defaultParserState description
      fmtDesc = writeHtml defaultWriterOptions doc

  defaultLayout $ do
    setTitle . toHtml $ "HackageDB mirror - Package - " <> pkgName
    $(widgetFile "package")

getCabal :: MonadIO m => Text -> Text -> m Text
getCabal pkgName pkgVersion = shelly $ do
  withTmpDir $ \dir -> chdir dir $ do
    run_ "tar" ["-xf", [lt|#{appDir}/package/#{pkgName}-#{pkgVersion}.tar.gz|] ]
    files <- S.find "."
    cabalPath <- maybe (fail "no cabal found") return $
      L.find (\p -> FP.extension p == Just "cabal") $ files
    LT.toStrict <$> readfile cabalPath
