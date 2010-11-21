-- | Makes an HTTP server that allows servlets to be registered.
module HTTP.Server ( Servlet(..), httpServlet, httpServer ) where

import HTTP.Request ( Request(..), getRequest )
import HTTP.Response ( Response, sendResponse, error400, error404 )

import Control.Concurrent ( forkIO )
import Control.Monad ( forever, unless, when )
import Data.Char ( toLower )
import Network ( withSocketsDo, listenOn, accept, PortID(PortNumber) )
import Network.HTTP.Headers ( HeaderName(..), findHeader )
import Network.URI ( URI, uriPath )
import System.FilePath.Glob ( Pattern, compile, match )
import System.IO ( hSetBuffering, hClose, Handle, BufferMode(..) )

-- | Servlet type.  This type allows complete generality in how we
-- respond, including allowing the possibility of setting up a
-- websocket.  The boolean return type signifies whether the
-- server should start listening for more requests on the same
-- handle.  The servlet is responsible for closing the handle
-- otherwise.
newtype Servlet = Servlet (Handle -> Request -> IO Bool)

-- | Makes a simple HTTP servlet, respecting the Connection header for
-- keeping sockets alive (though we don't support upgrading to websockets).
httpServlet :: (Request -> IO Response) -> Servlet
httpServlet f = Servlet serve
    where serve h r = do f r >>= sendResponse h
                         unless conn $ hClose h
                         return conn
              where conn = case (map toLower) `fmap` findHeader HdrConnection r of
                             Just "close" -> False
                             Just "keep-alive" -> True
                             _ -> rqVersion r == "HTTP/1.1"

-- | Launches an HTTP server with a list of servlets.  The patterns
-- are simple glob strings (compiled and matched with the Glob library)
-- and are matched in the order they appear.
httpServer :: Int                  -- ^ port number
           -> [(String, Servlet)]  -- ^ map of URI patterns to servlets
           -> IO ()
httpServer port servlets =
    do sock <- listenOn $ PortNumber $ fromIntegral port
       withSocketsDo $ forever $ do
         (h,n,_) <- accept sock -- ignoring the port number
         hSetBuffering h LineBuffering
         forkIO $ openSocket n h
  where openSocket :: String -> Handle -> IO ()
        openSocket n h =
            do mreq <- getRequest n h
               case mreq of
                 Just req -> do
                           --putStrLn $ show $ req { rqHeaders = [] }
                           putStrLn $ "Request "++show (rqMethod req)
                                    ++" "++take 60 (show (rqURI req))
                           serve n globs h req
                 Nothing  -> error400 >>= sendResponse h >> hClose h
        serve :: String -> [(Pattern, Servlet)] -> Handle -> Request -> IO ()
        serve n ((g,Servlet f):gs) h req
            | matches g req = f h req >>= (flip when $ openSocket n h)
            | otherwise = serve n gs h req
        serve _ [] h _ = error404 >>= sendResponse h >> hClose h
        globs :: [(Pattern, Servlet)]
        globs = map (\(pat, s) -> (compile pat, s)) servlets
        matches :: Pattern -> Request -> Bool
        matches g req = maybe False (match g . uriPath) $ rqURI req
