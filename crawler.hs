{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ViewPatterns         #-}

import qualified Blaze.ByteString.Builder           as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Codec.Archive.Tar                  as Tar
import           Codec.Archive.Tar.Entry            as Tar
import           Codec.Compression.GZip             as GZip
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8         as L
import           Data.Conduit
import           Data.Configurator                  as C
import           Data.Configurator.Types
import           Data.List
import           Data.Monoid
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as LT
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Database.Persist.Store
import qualified Filesystem                         as F
import qualified Filesystem.Path.CurrentOS          as FP
import           Network.HTTP.Conduit
import           Shelly
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           System.Locale
import           Text.Shakespeare.Text

import           Yesod.Default.Config

import           Model
import           Settings

default (Integer, LT.Text)

archiveName :: FP.FilePath -> String -> String -> FP.FilePath
archiveName appDir name ver =
  appDir </> "package" </> [st|#{name}-#{ver}|] <.> "tar.gz"

indexFile :: FP.FilePath -> FP.FilePath
indexFile appDir =
  appDir </> "00-index.tar.gz"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [conf] -> shelly . main' =<< C.load [Required conf]
    _ -> putStrLn "Usage: hackage-mirror <conf-file>"

main' :: Config -> Sh ()
main' conf = do
  home <- get_env_text "HOME"
  let appDir = home </> ".hackage"
  mkdir_p appDir

  initDB

  repo <- liftIO $ require conf "repo"
  updateMeta $ repo ++ "/log"

  newpacks <- whatsnew

  echo [lt|download #{show (length newpacks)} new packages|]

  mng <- liftIO $ newManager def

  forM_ (zip [1..] newpacks) $ \(ix, Entity key Package {..}) -> do
    let url = [st|#{repo}/#{packageName}/#{packageVersion}/#{packageName}-#{packageVersion}.tar.gz|]
        savedir = appDir </> "package"
        filename = savedir </> FP.fromText [st|#{packageName}-#{packageVersion}.tar.gz|]

    mkdir_p savedir
    echo [lt|[#{show ix}/#{show $ length newpacks}] downloading #{url}...|]
    download mng (T.unpack url) filename
    runDB $ update key [ PackageDownloaded =. True ]
    `catchany_sh` (\e -> echo_err $ LT.pack $ show e)

  liftIO $ closeManager mng

  makeZZIndex appDir
  makeLog appDir

makeZZIndex :: FP.FilePath -> Sh ()
makeZZIndex appDir = do
  echo "building 00-index.tar.gz..."
  pkgs <- runDB $ selectList [] [ Asc PackageName ]
  entries <- forM pkgs $ \(Entity _ Package{..}) -> do
    let arcname =
          archiveName appDir (T.unpack packageName) (T.unpack packageVersion)
        cabalname =
          T.unpack [st|/#{packageName}.cabal|]
        Right tarpath =
          toTarPath False $ T.unpack [st|#{packageName}/#{packageVersion}/#{cabalname}|]

    withFileSh arcname ReadMode $ \h -> do
      bs <- liftIO $ L.hGetContents h
      let loop e = case e of
            Tar.Done -> do
              echo_err [lt|#{toTextIgnore arcname}: cabal file not found|]
              return Nothing
            Tar.Fail err -> do
              echo_err [lt|#{show err}|]
              return Nothing
            Tar.Next (entry @ Entry { entryContent = NormalFile con _ }) _
                  | cabalname `isSuffixOf` entryPath entry -> do
                    let ccon = L.copy con
                    L.length ccon `seq` (return $ Just $ fileEntry tarpath ccon)
            Next _ next ->
              loop next
      loop $ Tar.read (GZip.decompress bs)

    `catchany_sh` (\e -> inspect e >> return Nothing)

  let tarball = GZip.compress $ Tar.write [ e | Just e <- entries ]
      tmpPath = indexFile appDir <> "-part"

  withFileSh tmpPath WriteMode $ \h -> liftIO $ L.hPut h tarball
  mv tmpPath (indexFile appDir)

makeLog :: FP.FilePath -> Sh ()
makeLog appDir = do
  echo "building log..."
  pkgs <- runDB $ selectList [] [ Asc PackageDate ]
  withFileSh (appDir </> "log") WriteMode$ \h -> liftIO $
    L.hPut h $ Blaze.toLazyByteString $ mconcat $ map (Blaze.fromText . printPkg) pkgs
  where
    printPkg (Entity _ Package{..}) =
      let tm = T.pack $ formatTime defaultTimeLocale "%c" packageDate
      in T.unwords [tm, packageUploader, packageName, packageVersion] <> "\n"

mvPool = unsafePerformIO $ newEmptyMVar
{-# NOINLINE mvPool #-}

initDB :: Sh ()
initDB = liftIO $ do
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

updateMeta :: String -> Sh ()
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

whatsnew :: Sh [Entity Package]
whatsnew =
  runDB $ selectList [PackageDownloaded ==. False] []

download :: Manager -> String -> FP.FilePath -> Sh ()
download mng url pa = do
  liftIO $ runResourceT $ do
    req <- parseUrl url
    Response {..} <- httpLbs req mng
    liftIO $ F.withFile pa WriteMode $ \h ->
      L.hPut h responseBody

withFileSh :: FP.FilePath -> IOMode -> (Handle -> Sh a) -> Sh a
withFileSh pa mode m = do
  h <- liftIO $ F.openFile pa mode
  m h `finally_sh` liftIO (hClose h)
