{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, DeriveDataTypeable, ViewPatterns #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Configurator as C
import Data.Configurator.Types
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Database.Persist
import qualified Filesystem.Path.CurrentOS as FP
import Network.HTTP.Conduit
import Shelly
import System.Environment
import System.Locale
import Text.Shakespeare.Text

import DB

main :: IO ()
main = do
  args <- getArgs
  case args of
    [conf] -> main' =<< C.load [Required conf]
    _ -> putStrLn "Usage: hackage-mirror <conf-file>"

main' :: Config -> IO ()
main' conf = shelly $ do
  home <- getenv "HOME"
  let appdir = home </> ".hackage"
  mkdir_p appdir
  initDB $ LT.toStrict $ toTextIgnore $ appdir </> "hackage.db3"
  
  repo <- liftIO $ require conf "repo"
  updateMeta $ repo ++ "/log"
  
  newpacks <- whatsnew
  echo $ LT.pack $ show (length newpacks) ++ " new packages"

  withManager $ \mng -> do
    forM_ newpacks $ \(Entity key Package {..}) -> do
      let url = [st|#{repo}/#{packageName}/#{packageVersion}/#{packageName}-#{packageVersion}.tar.gz|]
          savedir = appdir </> "archive" </> FP.fromText packageName </> FP.fromText packageVersion
          filename = savedir </> FP.fromText [st|#{packageName}-#{packageVersion}.tar.gz|]
      lift $ do
        mkdir_p savedir
        download mng (T.unpack url) filename
        runDB $ update key [ PackageDownloaded =. True ]
        `catchany_sh` (\e -> echo_err $ LT.pack $ show e)


updateMeta :: String -> ShIO () 
updateMeta url = do
  echo $ "downloading " <> LT.pack url <> " ..."
  logs <- L.unpack <$> simpleHttp url

  echo "updating database ..."
  forM_ (reverse $ lines logs) $ \line -> do
    let (version : name : uploader : (unwords . reverse -> tm)) = reverse $ words line
    case parseTime defaultTimeLocale "%c" tm of
      Just t -> do
        runDB $ void $ insert $ Package (T.pack name) (T.pack version) (T.pack uploader) t False
        `catchany_sh` (const $ return ())
      Nothing ->
        echo_err $ LT.pack $ "cannot parse: " ++ line
    return ()
  `catchany_sh` (\e -> echo_err $ LT.pack $ show e)

whatsnew :: ShIO [Entity Package]
whatsnew =
  runDB $ selectList [PackageDownloaded ==. False] []

download :: Manager -> String -> FP.FilePath -> ShIO ()
download mng url pa = do
  echo $ LT.pack $ "downloading " ++ url ++ "..."
  liftIO $ runResourceT $ do
    req <- parseUrl url
    Response {..} <- http req mng
    responseBody $$ CB.sinkFile (LT.unpack $ toTextIgnore pa)
