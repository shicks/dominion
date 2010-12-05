import Network.Websocket

main = startServer $ Config
       9876 Any Any 
                (\ws -> putStrLn "Open" >> send ws "hello")
                (\_ s -> putStrLn $ "Message: " ++ s)
                (\_ -> putStrLn "Close")

