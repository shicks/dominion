{-# LANGUAGE GeneralizedNewTypeDeriving #-}

module Dominion.Types where

import Control.Arrow ( first )

-- | Player ID is simply an int.
newtype PId = PId { unPId :: Int } deriving ( Eq, Ord, Show, Read )

-- | We use CardName as a placeholder in almost all cases, only pulling
-- the 'Card' itself out of the 'Game' monad when we absolutely need it.
-- This allows postponing evaluation of things that might change, such as
-- cost.
newtype CardName = CardName String deriving ( Eq, Ord )
instance Show CardName where
    shows (CardName c) = shows c
instance Read CardName where
    reads = map (first CardName) . reads

-- | Card ID numbers, corresponding to indexes into the card array.
newtype CId = CId { unCId :: Int } deriving ( Ix, Eq, Ord, Show, Read )

-- | Cards exist in a single location at once.  Locations may be owned
-- by a particular player, or may be communal.
data Location = Location { locationName :: String,
                           locationOwner :: Maybe Int }

-- | We store the actual information about what cards are where in an
-- array, thus enforcing a 1:1 relationship between cards and locations.
-- We also get ordering for free.  We use an immutable array because
-- (a) it allows us to use a pure state monad for the game state, and
-- (b) there are only a couple hundred cards, which only move about once
-- per second, so copying on each update is not prohibitive.
type CardArray = Array Int (CardName, Location)

-- | Basic error type for the 'Game' monad.
newtype GameError = GameError String deriving ( Eq, Show, Error )

-- | Immutable state for the 'Game' monad.  This is basically just
-- a list of player names and a bunch of communication channels.
-- The input chan presents some problems, since we need to respond
-- to lots of different requests in addition to waiting for answers.
data GameInput = GameInput {
      players      :: [(String, Output MessageToClient)],
      inputChan    :: Input MessageToServer,
      outputChan   :: Output RegisterQuestionMessage
    }

-- | There's a lot of data to keep track of in the game state.
-- In general we'll try to keep single-card effects as localized
-- as possible, but this often requires installing hooks in different
-- places and for different lengths of time.
data GameState = GameState {
      currentTurn :: PId,
      turnState :: TurnState,
      cardArray :: CardArray
    }

-- | The Game monad.  This is a combination Error/State/Reader/IO monad
-- (in that order, from the inside-out).
-- 
newtype Game a = Game {
      runGame :: GameInput -> GameState -> IO (Either GameError a,GameState)
    }

-- | Runs the game and returns just the result of the monad (or error).
evalGame :: Game a -> GameInput -> GameState -> IO (Either GameError a)
evalGame g i s = fst `fmap` runGame g i s

-- | Runs the game and returns the final state.
execGame :: Game a -> GameInput -> GameState -> IO GameState
execGame g i s = snd `fmap` runGame g i s

-- | Swallows errors silently.
try :: Game a -> Game ()
try a = catchError (a >> return ()) (\_ -> return ())

instance Monad Game where
    return a = Game $ \_ s -> return (Right a,s)
    -- The use of GameError, QId and CId below is just to eliminate a
    -- warning about these constructors being unused...
    -- fail e = Game $ \_ s -> return (Left (error e GameError QId CId),s)
    fail e = Game $ \_ s -> return (Left (error e), s)
    Game a >>= f = Game $ \i s -> do (a',s') <- a i s
                                     case a' of
                                       Left e -> return (Left e,s')
                                       Right a'' -> runGame (f a'') i s'

instance MonadState GameState Game where
    get = Game $ \_ s -> return (Right s,s)
    put s = Game $ \_ _ -> return (Right (),s)

instance MonadError GameError Game where
    throwError e = Game $ \_ s -> return (Left e,s)
    catchError (Game a) f = Game $ \i s -> do (a',s') <- a i s
                                              case a' of
                                                Left e -> runGame (f e) i s'
                                                _ -> return (a',s')

instance MonadReader Game where
    ask = Game $ \i s -> return (Right i, s)
    local f (Game a) = Game $ \i s -> a (f i) s

instance MonadIO Game where
    liftIO a = Game $ \_ s -> do { a' <- a; return (Right a',s) }

instance Functor Game where
    fmap f (Game a) = Game $ \_ s -> do (a',s') <- a s
                                        return (f `fmap` a',s')
