{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Settings

import Blaze.ByteString.Builder.Char.Utf8
import Codec.Compression.GZip as GZip
import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import qualified Data.Text as T
import Data.Time.Format
import Database.Persist.Store
import System.Directory
import System.IO
import System.Locale
import Text.Shakespeare.Text

appDir :: String
appDir = "/home/tanakh/.hackage"

indexFile :: String
indexFile = T.unpack [st|#{appDir}/archive/00-index.tar.gz|]

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

makeZZIndex :: App -> IO ()
makeZZIndex app = forever $ do
  putStrLn "building 00-index.tar.gz..." >> hFlush stdout
  let f = selectList [] [ Asc PackageName ]
  pkgs <- Database.Persist.Store.runPool (persistConfig app) f (connPool app)
  entries <- forM pkgs $ \(Entity _ Package{..}) ->
    E.handle (\E.SomeException{} -> return Nothing) $ do
      let arcname = archiveName packageName packageVersion
          cabalname = T.unpack [st|/#{packageName}.cabal|]
          Right tarpath =
            toTarPath False $ T.unpack [st|#{packageName}/#{packageVersion}/#{cabalname}|]
      withFile arcname ReadMode $ \h -> do
        bs <- L.readFile arcname

        let loop e = case e of
              Done -> do
                hPutStrLn stderr $ arcname ++ ": cabal file not found"
                return Nothing
              Fail err -> do
                hPrint stderr err
                return Nothing
              Next (entry @ Entry { entryContent = NormalFile con _ }) _
                    | cabalname `isSuffixOf` entryPath entry -> do
                      let ccon = L.copy con
                      L.length ccon `seq` (return $ Just $ fileEntry tarpath ccon)
              Next _ next ->
                loop next
        loop $ Tar.read (GZip.decompress bs)

  let tarball = GZip.compress $ Tar.write [ e | Just e <- entries ]
  L.writeFile (indexFile ++ "-part") tarball
  renameFile (indexFile ++ "-part") indexFile
  putStrLn "build 00-index.tar.gz done." >> hFlush stdout
  threadDelay $ 60 * 1000000

archiveName name ver =
  T.unpack [st|#{appDir}/archive/#{name}/#{ver}/#{name}-#{ver}.tar.gz|]

getPackageR :: Text -> Text -> Text -> Handler (ContentType, Content)
getPackageR name ver arc = do
  return ( "application/x-gzip"
         , ContentFile (T.unpack [st|#{appDir}/archive/#{name}/#{ver}/#{arc}|]) Nothing
         )
