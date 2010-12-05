{-# LANGUAGE TypeSynonymInstances #-}

-- | @HTTP.Chan@ is an interface module that effectively wraps access to
-- 'System.IO.Handle' allowing the use of one-way ('Input' or 'Output')
-- 'Chan's instead.
module HTTP.Chan ( Serializable(..), handle2io, handle2i, handle2o,
                   sink2o, source2i, pipe, interacting,
                   Input, readInput, getInput, isEmptyInput,
                   Output, writeOutput ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan,
                                 isEmptyChan, getChanContents, unGetChan )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar,
                                 takeMVar, isEmptyMVar )
import Control.Monad ( forever, when )
import Data.Maybe ( isJust, catMaybes )
import System.IO ( Handle, hPutStrLn, hGetLine )
import Text.JSON ( JSValue, Result(..), encode, decode )

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

-- how do we deal with closing these channels?
-- the only way to get a chan is source2i/sink2o or pipe
-- thus, ...
-- ... this probably breaks any possibility of dupping the input?
--   - look out for race conditions...
-- if we needed to we could possibly allow dupping with reference
-- counts...?  i.e. how many copies of the output channel are there
-- and only put Nothing when we close the last one...?  We'll have
-- to look into use cases for when we ever actually close chans
-- manually...?

instance Serializable JSValue where
    showLine = encode
    readLine x = case decode x of
                   Ok a -> Just a
                   Error _ -> Nothing

instance Serializable String where
    showLine = id
    readLine = Just

-- | Wraps a 'Chan' to make it read-only.
newtype Input a = Input (Chan (Maybe a), MVar ())

-- | Wraps a 'Chan' to make it write-only.
newtype Output a = Output (Chan (Maybe a), MVar ())

-- | Reads from an 'Input' channel.
readInput :: Input a -> IO a
readInput (Input (c,_)) =
    do ma <- readChan c
       case ma of
         Just a -> return a
         Nothing -> do unGetChan c ma
                       fail "EOF while reading input channel"

-- | Writes to an 'Output' channel.
writeOutput :: Output a -> a -> IO ()
writeOutput (Output (c,mv)) a =
    do open <- isEmptyMVar mv
       if open then writeChan c $ Just a
               else fail "Wrote to closed output channel"

-- | Determines whether the input channel is open
isInputOpen :: Input a -> IO Bool
isInputOpen (Input (c,_)) =
    do empty <- isEmptyChan c
       if empty then return True
                else do ma <- readChan c
                        unGetChan c ma
                        return $ isJust ma

-- | Determines whether the output channel is open
isOutputOpen :: Output a -> IO Bool
isOutputOpen (Output (_,mv)) = isEmptyMVar mv

-- | Closes the input channel
closeInput :: Input a -> IO ()
closeInput (Input (_,mv)) = putMVar mv ()

-- | Closes the output channel
closeOutput :: Output a -> IO ()
closeOutput (Output (c,mv)) = do open <- isEmptyMVar mv
                                 when (not open) $ do
                                   putMVar mv ()
                                   writeChan c Nothing

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
handle2o h = sink2o $ hPutStrLn h . showLine

-- | Given a readable 'Handle', creates an 'Input' chan and forks a
-- thread to copy the @Handle@'s content to the channel.
handle2i :: Serializable i => Handle -> IO (Input i)
handle2i h = source2i $ readLine `fmap` hGetLine h

-- | Given a sink, creates an 'Output' chan such that
-- all writes to the channel are sent to the sink
sink2o :: (o -> IO ()) -> IO (Output o)
sink2o sink = do o <- newChan
                 mv <- newEmptyMVar
                 forkIO $ forever (do Just x <- readChan o
                                      sink x)
                            `catch` (\_ -> putMVar mv ())
                 return (Output (o, mv))

-- | Given a source, creates an 'Input' chan and forks a
-- thread to copy the source's content to the channel.
-- The source returns Nothing on a bad chunk of data,
-- rather than waiting around for good data.
source2i :: (IO (Maybe i)) -> IO (Input i)
source2i source = do i <- newChan
                     mv <- newEmptyMVar -- unused
                     forkIO $ forever (do Just x <- source
                                          writeChan i $ Just x)
                                `catch` (\_ -> writeChan i Nothing)
                     return (Input (i, mv))

-- | Creates a new channel and returns the 'Input' and 'Output' ends.
pipe :: IO (Input a, Output a)
pipe = do c <- newChan
          mv <- newEmptyMVar
          return (Input (c, mv), Output (c, mv))

-- | Forks a thread to interact an 'Input' and an 'Output' chan via
-- an interaction function.
interacting :: Input i -> Output o -> ([i] -> [o]) -> IO ()
interacting (Input (i,_)) o f =
    do is <- getChanContents i
       forkIO $ mapM_ (writeOutput o) $ f $ catMaybes is
       return ()

-- | Lazily returns all the contents of an 'Input' chan
-- (i.e. wraps 'getChanContents').
getInput :: Input a -> IO [a] -- lazy
getInput (Input (i,_)) = catMaybes `fmap` getChanContents i

-- | Returns true if the 'Input' chan is empty (i.e.
-- wraps 'isEmptyChan').
isEmptyInput :: Input a -> IO Bool
isEmptyInput (Input (i,_)) = isEmptyChan i
