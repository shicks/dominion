-- | @HTTP.Chan@ is an interface module that effectively wraps access to
-- 'System.IO.Handle' allowing the use of one-way ('Input' or 'Output')
-- 'Chan's instead.
module HTTP.Chan ( ShowRead(..), handle2io, handle2i, handle2o,
                   pipe, interacting,
                   Input, readInput, getInput, isEmptyInput,
                   Output, writeOutput ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan,
                                 isEmptyChan, getChanContents )
import Control.Monad ( forever )
import System.IO ( Handle, hPutStrLn, hGetLine )
import Text.JSON ( JSValue )
import Text.JSON.Strings ( showJSValue, runGetJSON, readJSValue )

-- | Because 'Chan's are typed, we need to specify a wire protocol
-- in order to interface them with the string-based 'Handle' IO.
-- We define default a reasonable default implementation so that
-- any instance of both 'Show' and 'Read' should be instantiable
-- in 'Serializable' without any extra implementation.  We require
-- that serialized data not contain any newlines, since otherwise
-- we run the risk of infinite loops slurping more and more lines
-- to try to parse something that will never parse.
class (Show a, Read a) => Serializable a where
    showLine :: a -> String
    showLine = show
    readLine :: String -> Maybe a
    readLine x = do [(a,"")] <- Just $ reads x
                    Just a

instance Serializable JSValue where
    showLine = showJSValue
    readLine = either (const Nothing) Just . runGetJSON readJSValue

-- | Wraps a 'Chan' to make it read-only.
newtype Input a = Input (Chan a)

-- | Wraps a 'Chan' to make it write-only.
newtype Output a = Output (Chan a)

-- | Reads from an 'Input' channel.
readInput :: Input a -> IO a
readInput (Input c) = readChan c

-- | Writes to an 'Output' channel.
writeOutput :: Output a -> a -> IO ()
writeOutput (Output c) = writeChan c

-- | Given a read-write 'Handle', creates a pair of wrapped channels
-- that are tied to the handle.  All data read from the @Handle@ is
-- sent to the 'Input' chan, and all data written to the 'Output'
-- chan is sent to the @Handle@.
handle2io :: (Serializable i, Serializable o)
          => Handle -> IO (Input i, Output o)
handle2io h = do i <- handle2i h
                 o <- handle2o h
                 return (i, o)

-- | Given a writeable 'Handle', creates an 'Output' chan such that
-- all writes to the channel are sent to the @Handle@.
handle2o :: Serializable o => Handle -> IO (Output o)
handle2o h = do o <- newChan
                forkIO $ forever $ do x <- readChan o
                                      hPutStrLn h $ showLine x
                return (Output o)

-- | Given a readable 'Handle', creates an 'Input' chan and forks a
-- thread to copy the @Handle@'s content to the channel.
handle2i :: Serializable i => Handle -> IO (Input i)
handle2i h = do i <- newChan
                forkIO $ forever (do x <- hGetLine h
                                     case readLine x of
                                       Just a -> writeChan i a
                                       Nothing -> fail ("bad data: "++x))
                           `catch` (\_ -> return ())
                return (Input i)

-- | Creates a new channel and returns the 'Input' and 'Output' ends.
pipe :: IO (Input a, Output a)
pipe = do c <- newChan
          return (Input c, Output c)

-- | Forks a thread to interact an 'Input' and an 'Output' chan via
-- an interaction function.
interacting :: Input i -> Output o -> ([i] -> [o]) -> IO ()
interacting (Input i) o f =
    do is <- getChanContents i
       forkIO $ mapM_ (writeOutput o) $ f is
       return ()

-- | Lazily returns all the contents of an 'Input' chan
-- (i.e. wraps 'getChanContents').
getInput :: Input a -> IO [a] -- lazy
getInput (Input i) = getChanContents i

-- | Returns true if the 'Input' chan is empty (i.e.
-- wraps 'isEmptyChan').
isEmptyInput :: Input a -> IO Bool
isEmptyInput (Input i) = isEmptyChan i
