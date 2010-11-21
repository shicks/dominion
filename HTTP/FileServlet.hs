-- | Provides a basic file-serving servlet.
module HTTP.FileServlet ( fileServlet ) where

import HTTP.Request ( Request(..), Method(..) )
import HTTP.Response ( Response, blank200, ok200,
                       error400, error403, error404, error501 )
import HTTP.Server ( Servlet, httpServlet )

import Data.Char ( toLower )
import Data.List ( isPrefixOf )
import Network.URI ( URI(..) )
import System.Directory ( canonicalizePath, makeRelativeToCurrentDirectory,
                          doesFileExist, doesDirectoryExist )
import System.FilePath ( (</>), takeExtension )
import System.IO ( IOMode(ReadMode), hGetContents, openBinaryFile )

-- | Naively maps file extensions to content-types.
contentType :: String   -- ^ file extension (e.g. ".jpg")
            -> String
contentType s | map toLower s `elem` [".jpg",".jpeg"] = "image/jpeg"
              | map toLower s == ".png" = "image/png"
              | map toLower s == ".gif" = "image/gif"
              | map toLower s `elem` [".htm",".html"] = "text/html"
              | map toLower s == ".css" = "text/html"
              | map toLower s == ".js" = "text/javascript"
              | map toLower s == ".xml" = "text/xml"
              | otherwise = "text/plain"

-- | Constructs a response for a file under the given root path.
-- We need to take some care here that we only send files in the
-- root directory and lower.  We don't allow symlinks for now.
-- Maybe later we'll add a parameter for other allowed directories.
sendFile :: String   -- root path, relative to "." (e.g. "htdocs")
         -> String   -- requested file
         -> IO Response
sendFile root f = do --putStrLn $ "sendFile "++f
                     cf <- canonicalizePath $ root </> dropWhile (=='/') f
                     --putStrLn $ "  cf = "++cf
                     rf <- makeRelativeToCurrentDirectory cf
                     --putStrLn $ "  rf = "++rf
                     if root `isPrefixOf` rf
                       then send' rf
                       else error403
    where send' rf = do de <- doesDirectoryExist rf
                        --putStrLn $ "send' "++rf++": de="++show de
                        if de then do
                                let ix = rf </> "index.html" -- parametrize?
                                ie <- doesFileExist $ ix
                                if ie then send'' ix
                                      else error403 -- maybe sendDir rf
                              else do
                                fe <- doesFileExist rf
                                if fe then send'' rf else error404
          send'' rf = do h <- openBinaryFile rf ReadMode
                         s <- hGetContents h
                         let ext = takeExtension rf
                             ctype = contentType ext
                         ok200 ctype s

-- | The 'Servlet'
fileServlet :: String -> Servlet
fileServlet root = httpServlet servlet
    where servlet req = do case rqMethod req of
                             NotImplemented -> error501
                             OPTIONS -> blank200
                             _ -> case rqURI req of
                                    Nothing  -> error400
                                    Just uri -> sendURI uri
          sendURI uri = sendFile root $ uriPath uri
          -- pattern-match on (`isPrefixOf` uriPath uri)
