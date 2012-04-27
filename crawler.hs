{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, DeriveDataTypeable, ViewPatterns #-}

import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Codec.Compression.GZip as GZip
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Configurator as C
import Data.Configurator.Types
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.List
import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Database.Persist
import qualified Database.Persist.Store
import Database.Persist.Sqlite
import qualified Filesystem.Path.CurrentOS as FP
import Network.HTTP.Conduit
import Shelly
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Locale
import Text.Shakespeare.Text

import Yesod.Default.Config

import Model
import Settings

appDir :: String
appDir = "/home/tanakh/.hackage"

archiveName :: String -> String -> String
archiveName name ver =
  T.unpack [st|#{appDir}/package/#{name}-#{ver}.tar.gz|]

indexFile :: String
indexFile =
  T.unpack [st|#{appDir}/00-index.tar.gz|]

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
  
  initDB
  
  repo <- liftIO $ require conf "repo"
  updateMeta $ repo ++ "/log"

  newpacks <- whatsnew
  echo [lt|download #{show (length newpacks)} new packages|]

  withManager $ \mng -> do
    forM_ (zip [1..] newpacks) $ \(ix, Entity key Package {..}) -> do
      let url = [st|#{repo}/#{packageName}/#{packageVersion}/#{packageName}-#{packageVersion}.tar.gz|]
          savedir = appdir </> "package"
          filename = savedir </> FP.fromText [st|#{packageName}-#{packageVersion}.tar.gz|]
      lift $ do
        mkdir_p savedir
        echo [lt|[#{show ix}/#{show $ length newpacks}] downloading #{url}...|]
        download mng (T.unpack url) filename
        runDB $ update key [ PackageDownloaded =. True ]
        `catchany_sh` (\e -> echo_err $ LT.pack $ show e)

  makeZZIndex

makeZZIndex :: ShIO ()
makeZZIndex = do
  echo "building 00-index.tar.gz..."
  pkgs <- runDB $ selectList [] [ Asc PackageName ]
  entries <- forM pkgs $ \(Entity _ Package{..}) -> liftIO (do
    let arcname =
          archiveName (T.unpack packageName) (T.unpack packageVersion)
        cabalname =
          T.unpack [st|/#{packageName}.cabal|]
        Right tarpath =
          toTarPath False $ T.unpack [st|#{packageName}/#{packageVersion}/#{cabalname}|]

    withFile arcname ReadMode $ \h -> do
      bs <- L.hGetContents h
      let loop e = case e of
            Tar.Done -> do
              hPutStrLn stderr $ arcname ++ ": cabal file not found"
              return Nothing
            Tar.Fail err -> do
              hPrint stderr err
              return Nothing
            Tar.Next (entry @ Entry { entryContent = NormalFile con _ }) _
                  | cabalname `isSuffixOf` entryPath entry -> do
                    let ccon = L.copy con
                    L.length ccon `seq` (return $ Just $ fileEntry tarpath ccon)
            Next _ next ->
              loop next
      loop $ Tar.read (GZip.decompress bs)
    ) `catchany_sh` (\e -> inspect e >> return Nothing)

  let tarball = GZip.compress $ Tar.write [ e | Just e <- entries ]
      tmpPath = indexFile <> "-part"
  liftIO $ L.writeFile tmpPath tarball
  mv (fromString tmpPath) (fromString indexFile)

mvPool = unsafePerformIO $ newEmptyMVar
{-# NOINLINE mvPool #-}

initDB :: ShIO ()
initDB = lift $ do
  -- initialize DB connection
  dbconf <- withYamlEnvironment "config/sqlite.yml" Development
            Database.Persist.Store.loadConfig >>=
            Database.Persist.Store.applyEnv
  p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
  Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
  putMVar mvPool p

runDB :: MonadIO m => SqlPersist IO a -> m a
runDB sql =
  liftIO $ withMVar mvPool $ runSqlPool sql

updateMeta :: String -> ShIO () 
updateMeta url = do
  echo [lt|downloading #{url}...|]
  logs <- L.unpack <$> simpleHttp url

  echo "updating database ..."
  forM_ (reverse $ lines logs) $ \line -> do
    let (version : name : uploader : (unwords . reverse -> tm)) = reverse $ words line
    case parseTime defaultTimeLocale "%c" tm of
      Just t -> do
        runDB $ void $ Database.Persist.Sqlite.insert $ Package (T.pack name) (T.pack version) (T.pack uploader) t False
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
  liftIO $ runResourceT $ do
    req <- parseUrl url
    Response {..} <- http req mng
    responseBody $$ CB.sinkFile (LT.unpack $ toTextIgnore pa)
