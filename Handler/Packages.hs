{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Packages where

import Import

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Text.Regex.TDFA
import Yesod.Paginator

getPackagesR :: Handler RepHtml
getPackagesR = do
  (T.head . fromMaybe "A" -> curL) <- lookupGetParam "letter"
  (read . T.unpack . fromMaybe "1" -> page) <- lookupGetParam "p"

  let cur  = T.singleton curL :: Text
      suc  = T.singleton (succ curL) :: Text
  
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
      from = (page - 1) * ppp
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
                  find (\(Entity _ val) -> case pkgVersion of
                           Nothing -> True
                           Just version -> packageVersion val == version) pkg
  
  counts <- runDB $ do
    vers <- selectList [PackageName ==. pkgName] []
    forM vers $ \(Entity key val) -> do
      (packageVersion val, ) <$> count [DownloadPackage ==. key]

  let mycnt = fromMaybe 0 $ lookup (packageVersion pkg) counts
      totcnt = sum $ map snd counts

  defaultLayout $ do
    setTitle . toHtml $ "HackageDB mirror - Package - " <> pkgName
    $(widgetFile "package")
