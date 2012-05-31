{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Control.Monad
import qualified Data.Text as T
import Data.Time.Clock
import Text.Regex.TDFA
import Text.Shakespeare.Text

appDir :: String
appDir = "/home/tanakh/.hackage"

getHomeR :: Handler RepHtml
getHomeR = do
  defaultLayout $ do
    setTitle "HackageDB mirror - Home"
    $(widgetFile "homepage")

getLogR :: Handler (ContentType, Content)
getLogR = do
  return ("text/plain", ContentFile (T.unpack [st|#{appDir}/log|]) Nothing)

getZZIndexR :: Handler (ContentType, Content)
getZZIndexR =
  return ( "application/x-gzip"
         , ContentFile (T.unpack [st|#{appDir}/00-index.tar.gz|]) Nothing
         )

getPackageR :: Text -> Handler (ContentType, Content)
getPackageR arc = do
  now <- liftIO $ getCurrentTime

  [[_, pkgName, pkgVersion, _]] <- T.unpack arc =~~ ("(.*)-([0-9]+(\\.[0-9]+)+)\\.tar\\.gz$" :: String)
  runDB $ do
    Entity pkgKey _ <- getBy404 $ UniqueName (T.pack pkgName) (T.pack pkgVersion)
    void $ insert $ Download pkgKey now

  return ( "application/x-gzip"
         , ContentFile (T.unpack [st|#{appDir}/package/#{arc}|]) Nothing
         )
