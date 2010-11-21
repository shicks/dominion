-- | Defines a 'Response' data type and a number of helper functions
-- to make common responses.
module HTTP.Response ( Response(..), ok200,
                       continue100, blank200, blank204, redirect303,
                       error400, error403, error404, error405, error418,
                       error500, error501, error503,
                       sendResponse, sendResponseHdr,
                       htmlResponse, htmlResponse', jsResponse,
                       sanitize, jsPrintf
                     ) where

import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime )
import Network.HTTP.Headers ( Header(..), HeaderName(..), HasHeaders(..) )
import System.IO ( hPutStrLn, hPutStr, Handle )
import System.Locale ( defaultTimeLocale )

-- | The data type.
data Response = Response {
      rspVersion :: String,
      rspCode :: Int,
      rspReason :: String,
      rspHeaders :: [Header],
      rspBody :: String
} deriving ( Show )

instance HasHeaders Response where
    getHeaders (Response _ _ _ hs _) = hs
    setHeaders r hs = r { rspHeaders = hs }

-- | This is the main response-generating function
ok200 :: String    -- ^ content-type
      -> String    -- ^ response body
      -> IO Response
ok200 ctype body = do t <- date
                      return $ Response "HTTP/1.1" 200 "OK" (hs t) body
    where hs t = (m HdrContentType ctype):
                 (m HdrContentLength $ show $ length body):
                 (m HdrDate t):[]
          m = Header

-- * All sorts of other responses...
-- | A "Continue" (100) response
continue100 :: IO Response
continue100 = do t <- date
                 return $ Response "HTTP/1.1" 101 "Continue"
                            [Header HdrDate t] ""

-- | A blank "OK" (200) response.
blank200 :: IO Response
blank200 = do t <- date
              return $ Response "HTTP/1.1" 200 "OK"
                         [Header HdrDate t,Header HdrContentLength "0"] ""

-- | A blank "No Content" (204) response.
blank204 :: IO Response
blank204 = do t <- date
              return $ Response "HTTP/1.1" 204 "No Content"
                         [Header HdrDate t,Header HdrContentLength "0"] ""

-- | A "See Other" (303) response
redirect303 :: String    -- ^ location to redirect to
            -> IO Response
redirect303 l = htmlResponse' 303 "See Other" "HTTP/1.1" [loc] $ unlines
                ["<html><head><title>303 See Other</title>",
                 "</head><body><h1>See Other</h1>",
                 "<p>The resource you requested is located at",
                 "<a href="++l++">"++l++"</a>.  You should be",
                 "redirected there momentarily.</p>",
                 "</body></html>"]
    where loc = Header HdrLocation l

-- | A "Bad Request" (400) error
error400 :: IO Response
error400 = htmlResponse' 400 "Bad Request" "HTTP/1.1" [] $ unlines
           ["<html><head><title>400 Bad Request</title>",
            "</head><body><h1>Bad Request</h1>",
            "<p>Your client has issued a malformed or illegal request.</p>",
            "</body></html>"]

-- | A "Forbidden" (400) error
error403 :: IO Response
error403 = htmlResponse' 403 "Forbidden" "HTTP/1.1" [] $ unlines
           ["<html><head><title>403 Forbidden</title>",
            "</head><body><h1>Forbidden</h1>",
            "<p>You do not have permission to access this resource.</p>",
            "</body></html>"]

-- | A "Not Found" (404) error
error404 :: IO Response
error404 = htmlResponse' 404 "Not Found" "HTTP/1.1" [] $ unlines
           ["<html><head><title>404 Not Found</title>",
            "</head><body><h1>Not Found</h1>",
            "<p>The resource you requested could not be found.</p>",
            "</body></html>"]

-- | A "Method Not Allowed" (405) error
error405 :: IO Response
error405 = htmlResponse' 405 "Method Not Allowed" "HTTP/1.1" [] $ unlines
           ["<html><head><title>405 Method Not Allowed</title>",
            "</head><body><h1>Method Not Allowed</h1>",
            "<p>This resource does not allow the method you're using.</p>",
            "</body></html>"]

-- | An "I'm a teapot" (418) error
error418 :: IO Response
error418 = htmlResponse' 418 "I'm a teapot" "HTTP/1.1" [] $ unlines
           ["<html><head><title>418 I'm a teapot</title>",
            "</head><body><h1>I'm a teapot</h1>",
            "<p>I'm a little teapot, short and stout",
            "<br/>Here is my handle, here is my spout",
            "<br/>When I get excited, watch me shout",
            "<br/>Just tip me over and pour me out.</p>",
            "</body></html>"]

-- | An "Internal Server Error" (500) error
error500 :: IO Response
error500 = htmlResponse' 500 "Internal Server Error" "HTTP/1.1" [] $ unlines
           ["<html><head><title>500 Internal Server Error</title>",
            "</head><body><h1>Internal Server Error</h1>",
            "<p>Something went wrong.</p>",
            "</body></html>"]

-- | A "Not Implemented" (501) error
error501 :: IO Response
error501 = htmlResponse' 501 "Not Implemented" "HTTP/1.1" [] $ unlines
           ["<html><head><title>501 Not Implemented</title>",
            "</head><body><h1>Not Implemented</h1>",
            "<p>This server doesn't support that method.</p>",
            "</body></html>"]

-- | A "Service Unavailable" (503) error
error503 :: IO Response
error503 = htmlResponse' 503 "Service Unavailable" "HTTP/1.1" [] $ unlines
           ["<html><head><title>503 Service Unavailable</title>",
            "</head><body><h1>Service Unavailable</h1>",
            "<p>The service you requested is currently unavailable. ",
            "Please try again later.</p>",
            "</body></html>"]

-- * Helper methods
-- | Returns the current time in the proper format
date :: IO String
date = formatTime defaultTimeLocale "%a, %d %b %Y %X %Z" `fmap` getCurrentTime

-- | Sends a response to a handle.
sendResponse :: Handle -> Response -> IO ()
sendResponse h (Response v c r hs b) = do hPutStrLn h $ v++" "++show c++" "++r
                                          mapM_ (hPutStr h . show) hs
                                          hPutStrLn h ""
                                          hPutStrLn h b
                                          putStrLn $ "Response "++show c++" "
                                                       ++take 60 (show b)

-- | Sends just the code and headers - useful for streaming?!?
sendResponseHdr :: Handle -> Response -> IO ()
sendResponseHdr h (Response v c r hs _) =
    do hPutStrLn h $ v++" "++show c++" "++r
       mapM_ (hPutStr h . show) hs
       hPutStrLn h ""

-- | Makes an OK (200) response with content-type "text/html".
htmlResponse :: String -> IO Response
htmlResponse = ok200 "text/html; charset=UTF-8"

-- | Makes an OK (200) response with content-type "text/javascript".
jsResponse :: String -> IO Response
jsResponse = ok200 "text/javascript; charset=UTF-8"

-- | Makes a response with content-type "text/html".
htmlResponse' :: Int       -- ^ response code (i.e. 404)
              -> String    -- ^ reason string (i.e. "Not Found")
              -> String    -- ^ HTTP version (i.e. "HTTP/1.1")
              -> [Header]  -- ^ headers
              -> String    -- ^ HTML response body
              -> IO Response
htmlResponse' c r v hs0 b = do t <- date
                               return $ Response v c r (hs t) b
    where hs t = (m HdrContentType "text/html; charset=UTF-8"):
                 (m HdrContentLength $ show $ length b):
                 (m HdrDate t):hs0
          m = Header

-- * These are useful for making responses...

-- | Sanitizes a string for javascript by escaping
-- all the '@"@' and '@\@' characters.
sanitize :: String -> String
sanitize [] = []
sanitize ('\\':s) = '\\':'\\':sanitize s
sanitize ('"':s) = '\\':'"':sanitize s
sanitize (s:ss) = s:sanitize ss

-- | Sanitizes and substitutes strings into "@%s@" placeholders
-- in the pattern (which is itself not sanitized).
jsPrintf :: String    -- ^ patten
         -> [String]  -- ^ replacements
         -> String
jsPrintf "" _ = ""   -- TODO(sdh): can we log a warning?
jsPrintf ('%':'s':cs) (s:ss) = '"':sanitize s++'"':jsPrintf cs ss
jsPrintf ('%':'s':cs) [] = '"':'"':jsPrintf cs [] -- TODO(sdh): log? fail?
jsPrintf (c:cs) ss = c:jsPrintf cs ss
