{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE KindSignatures, TypeFamilies, EmptyDataDecls, FlexibleContexts, GADTs #-}
module DB where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.IO.Unsafe

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Package
  name Text
  version Text
  uploader Text
  date UTCTime
  downloaded Bool
  UniqueName name version
  deriving (Show)
|]

mvPool = unsafePerformIO $ newEmptyMVar
{-# NOINLINE mvPool #-}

initDB :: MonadIO m => Text -> m ()
initDB db = liftIO $ do
  pool <- createSqlitePool db 4
  runSqlPool (runMigration migrateAll) pool
  putMVar mvPool pool

runDB :: MonadIO m => SqlPersist IO a -> m a
runDB sql =
  liftIO $ withMVar mvPool $ runSqlPool sql
