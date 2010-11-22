module Main where

import HTTP.Chan ( Input, Output, readInput, writeOutput )
import HTTP.Request ( Request )
import HTTP.Server ( Servlet, httpServer )
import HTTP.FileServlet ( fileServlet )
import HTTP.WebSocket ( webSocket )

import Control.Concurrent ( forkIO )
import Control.Monad ( forever )

echoServlet :: Request -> Input String -> Output String -> IO ()
echoServlet req i o = do putStrLn "Got a WS request"
                         writeOutput o "Do you read me?"
                         forkIO $ forever echo `catch` (\_ -> putStrLn "echo threw" >> return ())
                         return ()
    where echo :: IO ()
          echo = do putStrLn "echo"
                    readInput i >>= (writeOutput o . (++", miaow"))


servlets :: [(String, Servlet)]
servlets = [("/echo", webSocket echoServlet)
           ,("/*", fileServlet "www")
           ]

main = httpServer 8080 servlets
