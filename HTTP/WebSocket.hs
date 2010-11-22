-- | Sets up a servlet that uses websockets.
module HTTP.WebSocket ( webSocket ) where

import HTTP.Chan ( Serializable(..), Input, Output, sink2o, source2i )
import HTTP.Request ( Request(..) )
import HTTP.Response ( error400, sendResponse )
import HTTP.Server ( Servlet(..) )

import Control.Concurrent ( forkIO )
import Control.Monad ( forever, (<=<) )
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 ( toString, fromString ) -- this is from utf8-string
import Data.Char ( chr )
import Network.HTTP.Headers ( HeaderName(..), findHeader )
import Network.URI ( URI(..) )
import Network.WebSockets ( getFrame, putFrame, putResponse )
import qualified Network.WebSockets as WS
import System.IO ( Handle, hClose )

webSocket :: (Serializable i, Serializable o)
          => (Request -> Input i -> Output o -> IO ())
          -> Servlet
webSocket = Servlet . serveWebSocket

serveWebSocket :: (Serializable i, Serializable o)
               => (Request -> Input i -> Output o -> IO ()) -- handler function
               -> Handle -> Request -> IO Bool
serveWebSocket handle h r = tryServe `catch` badRequest >> return False
    where tryServe :: IO ()
          tryServe = do
            wsReq <- getRequest
            putResponse h wsReq
            o <- sink2o $ putFrame' . showLine
            i <- source2i $ readLine `fmap` getFrame'
            handle r i o

          getRequest :: IO WS.Request -- throws on a bad request
          getRequest = do
            Just host <- return $ findHeader HdrHost r
            Just key1 <- return $ findHeader (HdrCustom "Sec-WebSocket-Key1") r
            Just key2 <- return $ findHeader (HdrCustom "Sec-WebSocket-Key2") r
            Just origin <- return $ findHeader (HdrCustom "Origin") r
            Just path <- return $ uriPath `fmap` rqURI r
            token <- (map (chr.fromIntegral) . BL.unpack) `fmap` BL.hGet h 8
            return $ WS.Request host path origin key1 key2 token

          badRequest :: e -> IO ()
          badRequest _ = error400 >>= sendResponse h >> hClose h

          getFrame' :: IO String
          getFrame' = (nonEmpty . toString) =<< getFrame h

          putFrame' :: String -> IO ()
          putFrame' = putFrame h . fromString

          nonEmpty :: String -> IO String -- id, but throws on an empty string
          nonEmpty [] = fail "empty"
          nonEmpty s = return s
