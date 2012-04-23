{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, DeriveDataTypeable #-}

import qualified Codec.Archive.Tar as Tar
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.Acid
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Configurator as C
import Data.Configurator.Types
import Data.Data
import Data.Maybe
import qualified Data.Text.Lazy.IO as LT
import Distribution.ModuleName hiding (main)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text as DT
import System.Directory
import System.Environment
import System.IO
import System.Log
import System.Log.Formatter
import System.Log.Handler
import System.Log.Handler.Simple
import System.Log.Logger as Log
import Text.Blaze.Html5 as Html5
import Text.Blaze.Renderer.Text

main :: IO ()
main = do
  hdlr <- streamHandler stdout DEBUG
  updateGlobalLogger "" $
    L.setLevel DEBUG .
    setHandlers [setFormatter hdlr (simpleLogFormatter "[$time $prio] $msg")]

  args <- getArgs
  case args of
    [conf] -> do
      main' =<< C.load [Required conf]
    _ -> putStrLn "Usage: hackage-mirror conf"

main' :: Config -> IO ()
main' conf = do
  openLocalStateFrom "~/.hackage/" M.empty
  -- site <- require conf "site"
  crawl conf

crawl :: Config -> IO ()
crawl conf = do
  repo <- require conf "repo" :: IO String
  site <- require conf "site" :: IO String
  archive <- require conf "archive" :: IO String

  descs <- getDescs conf

  -- download archives
  forM_ descs $ \GenericPackageDescription {..} -> do
    let pi @ PackageIdentifier {..} = package packageDescription
        dir = DT.display pkgName ++ "/" ++ DT.display pkgVersion
    exist <- doesFileExist $ archive ++ "/" ++ dir ++ "/" ++ DT.display pi ++ ".tar.gz"
    when (not (null $ DT.display pkgName) && not exist) $ try_ $ do
      infoM "" $ DT.display pi
      withDir (archive ++ "/" ++ dir) $ do
        BL.putStrLn =<< [cmd|wget #{repo}/#{dir}/#{DT.display pi}.tar.gz|]

  -- generate package pages
  forM_ descs $ \desc -> do
    when (not $ null $ DT.display $ pkgName $ package $ packageDescription desc) $ do
      let pname = DT.display $ package $ packageDescription desc
          dir = site ++ "/package/" ++ pname
      infoM "" $ "generating page for " ++ pname
      [cmd|mkdir -p #{dir}|]
      withDir dir $ do
        print desc
        LT.writeFile "index.html" $ renderHtml $ genPackageInfoPage desc

genPackageInfoPage :: GenericPackageDescription -> Html
genPackageInfoPage GenericPackageDescription {..} = docTypeHtml $ do
  let PackageDescription {..} = packageDescription
      pack @ PackageIdentifier {..} = package
      pname = DT.display pack

  Html5.head $ do
    title $ string $ pname ++ " - Hackage Mirror"

  Html5.body $ do
    h1 $ string $ DT.display pkgName ++ ": " ++ synopsis
    p $ string description
    
    table $ do
      tr $ td "Licence"    >> td (string $ show license)
      tr $ td "Copyright"  >> td (string copyright)
      tr $ td "Maintainer" >> td (string maintainer)
      tr $ td "Author"     >> td (string author)
      tr $ td "Stability"  >> td (string stability)
      tr $ td "Category"   >> td (string category)

    flip (maybe $ return ()) library $ \lib -> do
      ul $ do
        forM_ (exposedModules lib) $ \mname -> do
          li $ string $ DT.display mname

    flip (maybe $ return ()) condLibrary $ \clib -> do
      ul $ do
        forM_ (modules clib) $ \mname -> do
          li $ string $ DT.display mname

modules :: CondTree v c Library -> [ModuleName]
modules CondNode {..} =
  libModules condTreeData
  -- todo extract all

withDir :: FilePath -> IO a -> IO a
withDir dir m = do
  [cmd|mkdir -p #{dir}|]  
  bracket
    (getCurrentDirectory)
    (setCurrentDirectory)
    (\_ -> setCurrentDirectory dir >> m)

getDescs :: Config -> IO [GenericPackageDescription]
getDescs conf = do
  {-
  repo <- require conf "repo"
  let ixUrl = repo ++ "/00-index.tar.gz"
  req <- parseUrl ixUrl
  infoM "" $ "downloading " ++ ixUrl
  tarIndex <- responseBody <$> withManager $ \mng -> E.run_ $ httpLbs req { rawBody = False } mng
  -}
  tarIndex <- BL.readFile "./00-index.tar"
  let es = Tar.foldEntries (:) [] (\err -> error $ show err) $ Tar.read tarIndex
  catMaybes <$> mapM processEntry es

processEntry :: Tar.Entry -> IO (Maybe GenericPackageDescription)
processEntry e = case Tar.entryContent e of
  Tar.NormalFile content length -> do
    BL.putStr content
    gp <- failIO $ parsePackageDescription (BL.unpack content)
    return $ Just gp
  _ ->
    return Nothing

data CabalParseError = CabalParseError String
  deriving (Show, Data, Typeable)

instance Exception CabalParseError

failIO :: ParseResult a -> IO a
failIO result = case result of
  ParseFailed err ->
    throwIO $ CabalParseError (show err)
  ParseOk warns ret -> do
    forM warns $ \warn -> 
      warningM "parse error" (show warn)
    return ret

try_ :: IO a -> IO ()
try_ m = void m `E.catch` (\(SomeException e) -> errorM "" (show e))
