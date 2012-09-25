{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Import

import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Maybe
import Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import qualified Network.Wai as Wai

postUploadR :: Handler ()
postUploadR = undefined

{-
postUploadR :: Handler ()
postUploadR = do
  hreq <- liftIO . toHTTPRequest =<< waiRequest
  resp <- liftIO $ withManager $ \mng -> do
    r <- http hreq mng
    toWaiResponse r
  sendWaiResponse resp

toHTTPRequest :: MonadResource m
                 => Wai.Request
                 -> IO (HTTP.Request m)
toHTTPRequest req = do
  let len = fromMaybe 0 $ do
        bs <- lookup "content-length" $ Wai.requestHeaders req
        (l, _) <- listToMaybe $ reads $ B.unpack bs
        return $ min (2^(24::Int)) l -- maximum 16MB

  body <- runResourceT $ Wai.requestBody req $$ CB.take len

  return $ HTTP.def
    { method = Wai.requestMethod req
    , host = "hackage.haskell.org"
    , port = 80
    , path = "/packages/upload"
    , queryString = renderQuery False $ Wai.queryString req
    , requestHeaders =
      filter ((/= "host") . fst) $
      filter ((/= "content-length") . fst) $
      Wai.requestHeaders req
    , requestBody = RequestBodyLBS body
    , checkStatus = \_ _ -> Nothing
    }

toWaiResponse :: MonadResource m
                 => Response (Source m B.ByteString)
                 -> m Wai.Response
toWaiResponse resp = do
  undefined
  {-
  responseBody resp $$ CB.take (2^(24::Int))
  return $ Wai.responseLBS
    (responseStatus resp)
    (responseHeaders resp)
    body
  -}

-}
