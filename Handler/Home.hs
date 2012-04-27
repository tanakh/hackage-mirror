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

indexFile :: String
indexFile =
  T.unpack [st|#{appDir}/00-index.tar.gz|]

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getLogR :: Handler RepPlain
getLogR = do
  pkgs <- runDB $ selectList [] [ Asc PackageDate ]
  return $ RepPlain $ ContentBuilder (mconcat $ map (fromText . printPkg) pkgs) Nothing
  where
    printPkg (Entity _ Package{..}) =
      let tm = T.pack $ formatTime defaultTimeLocale "%c" packageDate
      in T.unwords [tm, packageUploader, packageName, packageVersion] <> "\n"

getZZIndexR :: Handler (ContentType, Content)
getZZIndexR =
  return ("application/x-gzip", ContentFile indexFile Nothing)

getPackageR :: Text -> Handler (ContentType, Content)
getPackageR arc = do
  return ( "application/x-gzip"
         , ContentFile (T.unpack [st|#{appDir}/package/#{arc}|]) Nothing
         )
