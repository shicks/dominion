module Main where

import HTTP.Server ( Servlet, httpServer )
import HTTP.FileServlet ( fileServlet )

servlets :: [(String, Servlet)]
servlets = [("/*", fileServlet "www")]

main = httpServer 8080 servlets
