{-# LANGUAGE PatternGuards #-}

-- | Defines a 'Request' data type and a number of getters to read
-- the details (headers, URI) of the request.  The existing request
-- libraries on Hackage are more geared towards creating requests,
-- whereas we need to read a request from stdin and figure out
-- what's being asked for.
module HTTP.Request ( Request(..), Method(..),
                      getRequest, urlDecode
                    ) where

import Data.Char ( isSpace, chr )
import Control.Monad ( replicateM )
import Network.HTTP.Headers ( Header(..), HeaderName(..), HasHeaders(..),
                              parseHeaders, findHeader )
import Network.URI ( URI(..),{- URIAuth(..),-} parseURIReference )
import System.IO ( hGetLine, hGetChar, Handle)
import Text.ParserCombinators.Parsec ( Parser, parse, many, many1,
                                       noneOf, skipMany,
                                       digit, char, eof,
                                       (<|>), (<?>), try, choice,
                                       satisfy, string )

-- | The data type.
data Request = Request {
      rqVersion :: String,
      rqMethod :: Method,
      rqURI :: Maybe URI,
      rqHeaders :: [Header],
      rqFrom :: String,
      rqBody :: String
} deriving ( Show )

instance HasHeaders Request where
    getHeaders (Request _ _ _ hs _ _) = hs
    setHeaders r hs = r { rqHeaders = hs }

-- | Possible methods.
data Method = GET | HEAD | POST | OPTIONS
            | NotImplemented deriving ( Show, Read, Eq )

-- * High-level functions

-- | Get a request from the input handle if possible.  We don't obey
-- the "100-continue" convention yet, but we should eventually.
getRequest :: String    -- ^ hostname of client (saved in request)
           -> Handle    -- ^ handle to read request from
           -> IO (Maybe Request)
getRequest n h = do l <- getLine'' -- skip blank lines
                    case parseSimpleRequest l of
                      Just r -> ret r
                      Nothing -> do
                        ls <- getLines -- read until \n\n
                        case parseRequest $ unlines $ l:ls of
                          Nothing -> return Nothing
                          Just rq
                            | rqMethod rq /= POST -> ret rq 
                            | otherwise -> ret =<< readBody rq
    where ret r = return $ Just r { rqFrom = n }
          getLines = do l <- getLine'
                        if null l then return [""]
                                  else (l:) `fmap` getLines
          getLine'' = do l <- getLine'
                         if null l then getLine'' else return l
          getLine' = do l <- filter (not . (`elem`"\r\n")) `fmap` hGetLine h
                        return l
          getBytes b = replicateM b $ hGetChar h -- really should use hGetBuf
          readBody rq = case slen of
                          Just [(len,"")] -> do b <- getBytes len
                                                return $ rq { rqBody = b }
                          _ -> return rq
              where mlen = findHeader HdrContentLength rq
                    slen = readsPrec 0 `fmap` mlen

-- | Decode a URL query string into an associative list, unescaping
-- any @%xx@ codes and turning "@+@" into space.
urlDecode :: String -> [(String,String)]
urlDecode = ud "" . dropWhile (=='?')
    where ud k ('&':xs) = (reverse k,""):ud "" xs
          ud k ('=':xs) = ud' (reverse k) "" xs
          ud k ('%':a:b:xs) | [(x,"")] <- readsPrec 0 $ '0':'x':a:b:[]
                                       = ud (chr x:k) xs
          ud k ('+':xs) = ud (' ':k) xs
          ud k (x:xs) = ud (x:k) xs
          ud k [] = [(reverse k,"")]
          ud' k v ('&':xs) = (k,reverse v):ud "" xs
          ud' k v ('%':a:b:xs) | [(x,"")] <- readsPrec 0 $ '0':'x':a:b:[]
                                          = ud' k (chr x:v) xs
          ud' k v ('+':xs) = ud' k (' ':v) xs
          ud' k v (x:xs) = ud' k (x:v) xs
          ud' k v [] = [(k,reverse v)]

-- * Low-level parsing functions.  These shouldn't be used externally.

-- | Attempts to parse a simple HTTP/1.0 request that doesn't specify
-- an HTTP version.  Because the server should respond immediately,
-- rather than waiting for a second newline, this is broken out
-- separately.
parseSimpleRequest :: String -> Maybe Request
parseSimpleRequest s = case parse simpleRequest "" s of
                         Left _e -> Nothing
                         Right r -> Just r

-- | Parses a full HTTP request.  If parsing fails, we should throw
-- more lines at it.
parseRequest :: String -> Maybe Request
parseRequest s = case parse request "" s of
                   Left _e -> Nothing
                   Right r -> Just r

-- * Parsec 'Parser's.

-- | 'Parser' for an HTTP request, either simple of full.
request :: Parser Request
request = simpleRequest <|> fullRequest

-- | 'Parser' for a simple HTTP/1.0 request, of the form "GET /foo".
simpleRequest :: Parser Request
simpleRequest = try $ do string "GET"
                         spaces
                         uri <- readURI
                         spaces >> eof
                         return $ Request "HTTP/1.0" GET uri [] "" ""

-- | 'Parser' for a full HTTP request, i.e. "GET /foo HTTP/1.0"
fullRequest :: Parser Request
fullRequest = do meth <- method
                 uri <- readURI
                 ver <- readVersion
                 newline
                 hdrs <- headers
                 newline
                 -- body <- readBody meth hdrs
                 return $ Request ver meth uri hdrs "" ""

-- | Parses a 'URI'.  Since some methods don't have URI but
-- specify "@*@" instead, we return a @Maybe URI@ rather than
-- outright failing in that one exception.
readURI :: Parser (Maybe URI)
readURI = do s <- many (satisfy (not . isSpace)) <?> "URI"
             spaces
             case parseURIReference s of
               Just u -> return $ Just u
               Nothing -> if s=="*"
                          then return Nothing
                          else fail "no parse: URI"

-- | Parses a 'Method'
method :: Parser Method
method = do s <- many1 $ satisfy $ not . isSpace
            spaces
            return $ case s of
                       "GET" -> GET
                       "HEAD" -> HEAD
                       "POST" -> POST
                       "OPTIONS" -> OPTIONS
                       _ -> NotImplemented

-- | Parses "@HTTP/x.y@"
readVersion :: Parser String
readVersion = do string "HTTP/"
                 major <- many1 digit
                 char '.'
                 minor <- many1 digit
                 spaces
                 return $ "HTTP/"++major++"."++minor

-- | Parses the headers.
headers :: Parser [Header]
headers = do ls <- many nonEmpty
             case parseHeaders ls of
               Left e -> fail $ show e
               Right hs -> return hs
    where nonEmpty = do l <- many1 $ noneOf "\r\n"
                        newline
                        return l

-- | Skips literal spaces, but not newlines (which is why we can't
-- just use the built-in spaces function).
spaces :: Parser ()
spaces = skipMany $ char ' '

-- | Parses a single newline.
newline :: Parser ()
newline = choice [char '\n' >> ((char '\r' >> return ()) <|> return ()),
                  char '\r' >> ((char '\n' >> return ()) <|> return ())]
