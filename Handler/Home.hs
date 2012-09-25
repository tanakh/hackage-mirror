{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home where

import Import

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Time.Clock
import Database.Persist.GenericSql.Raw
import Database.Persist.Store
import Text.Shakespeare.Text
import Yesod.Default.Config

takeM :: (Functor m, Monad m) => Int -> [a] -> (a -> m Bool) -> m [a]
takeM 0 _ _ = return []
takeM _ [] _ = return []
takeM n (x:xs) p = do
  b <- p x
  if b
    then (x:) <$> takeM (n-1) xs p
    else takeM n xs p

getHomeR :: Handler RepHtml
getHomeR = do
  now <- liftIO getCurrentTime
  root <- appRoot . settings <$> getYesod

  let sanitize = T.map (\c -> if c == ':' then '-' else c)
  let (sanitize -> host)
        | "http://"  `T.isPrefixOf` root = T.drop 7 root
        | "https://" `T.isPrefixOf` root = T.drop 8 root
        | otherwise = root

  (map entityVal -> recent) <- runDB $ do
    recent <- selectList [PackageDownloaded ==. True] [Desc PackageDate, LimitTo 100]
    takeM 5 recent $ \(Entity _ val) -> do
      cnt <- count [PackageName ==. packageName val, PackageDate <=. packageDate val]
      return $ cnt == 1
  
  (map entityVal -> updated) <- runDB $ do
    recent <- selectList [PackageDownloaded ==. True] [Desc PackageDate, LimitTo 100]
    takeM 5 recent $ \(Entity _ val) -> do
      cnt <- count [PackageName ==. packageName val, PackageDate <=. packageDate val]
      return $ cnt > 1
  
  let cvt ls =
        [ (a, b, fromIntegral c)
        | [PersistText a, PersistText b, PersistInt64 c] <- ls
        ]
  (cvt -> pop) <- runDB $ do
    C.runResourceT $ withStmt [st|
SELECT "Package".name, "Package".version, count("Download")
FROM "Package", "Download"
WHERE "Package".id == "Download".package
AND "Download".date >= ?
GROUP BY name
ORDER BY count("Download") DESC
LIMIT 5
|] [PersistUTCTime $ (-24*60*60) `addUTCTime` now] C.$$ CL.consume

  let showDate date
        | sec < 1   = "just now" :: String
        | sec < 60  = show (floor sec) ++ " seconds ago"
        | mnu < 60  = show (floor mnu) ++ " minutes ago"
        | hou < 24  = show (floor hou) ++ " hours ago"
        | otherwise = show (floor day) ++ " days ago"
        where
          sec = realToFrac (now `diffUTCTime` date) :: Double
          mnu = sec / 60
          hou = mnu / 60
          day = hou / 24

      specName pkg =
        packageName pkg <> "-" <> packageVersion pkg
  
  defaultLayout $ do
    setTitle "HackageDB mirror - Home"
    $(widgetFile "homepage")
