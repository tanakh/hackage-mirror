{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Blaze.ByteString.Builder.Char.Utf8
import qualified Data.Text as T
import Data.Time.Format
import System.Locale
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
  return ( "application/x-gzip"
         , ContentFile (T.unpack [st|#{appDir}/package/#{arc}|]) Nothing
         )
