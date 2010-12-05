-- | Protocol module for Dominion.  Defines all the data types sent
-- over the wire.  We might want to figure out a way to integrate
-- hprotoc here, but since we're serializing to json rather than pb,
-- we can't use it directly.
module Dominion.Proto where

import Control.Applicative ( (<$>), (<*>) )
import Text.JSON ( JSON(..) )

noParse :: JSON a => String -> JSValue -> Result a
noParse typename value = fail $ "Unable to parse " ++ typename ++ ": " ++ show value

rj :: JSON a => JSValue -> Result a
rj = readJSON

sj :: JSON a => a -> JSValue
sj = showJSON

unpack :: JSValue -> [JSValue]
unpack (JSArray xs) = xs
unpack _ = undefined

pack :: [JSValue] -> JSValue
pack xs = JSArray xs

-- | Player ID...?
newtype PId = PId Integer

-- | Client-to-server messages, always serialized as an array so it
-- can be passed as a top-level JSON type.  Response messages have a
-- request ID that must match the original question, to make sure
-- delayed responses don't mess anything up.
data ClientMessage = GetStatus              -- ^ Asks for a status update
                   | PickOptions Int [Int]  -- ^ Picks numerical choice
                   | PickCards Int [String] -- ^ Picks cards
instance JSON ClientMessage where
    showJSON GetStatus = JSArray [sj "s"]
    showJSON (PickOptions i os) = pack $ sj "o" : sj i : map sj os
    showJSON (PickCards i cs) = pack $ sj "c" : sj i : map sj cs
    readJSON (JSArray [JSString "s"] = return GetStatus
    readJSON (JSArray (JSString "o":i:xs)) = PickOptions <$> rj i <*> rj xs
    readJSON (JSArray (JSString "c":i:xs)) = PickCards <$> rj i <*> rj xs
    readJSON json = noParse "ClientMessage" json

type PId = Int -- for now...?

-- | Server-to-client messages, always serialized as an array so it
-- can be passed as a top-level JSON type.
data ServerMessage = StatusReply DominionStatus
                   | UpdateMessage DominionUpdate
                   | ServerQuestion Int ServerQuestion
                   --  | Ack Int -- ^ response acknowledged...?
instance JSON ServerMessage where
    showJSON (StatusReply s) = pack $ sj "s" : unpack (sj s)
    showJSON (UpdateMessage u) = pack $ sj "u" : unpack (sj u)
    showJSON (ServerQuestion i q) = pack $ sj "q" : sj i : unpack (sj q)
    readJSON (JSArray (JSString "s":xs)) = StatusReply <$> rj (pack xs)
    readJSON (JSArray (JSString "u":xs)) = UpdateMessage <$> rj (pack xs)
    readJSON (JSArray (JSString "q":xs)) = ServerQuestion <$> rj (pack xs)
    readJSON json = noParse "ServerMessage" json

-- | Messages server pushes to client about an update
data DominionUpdate = ShuffleEvent PId -- ^ somebody shuffled
                    | NewTurn PId -- ^ the turn changed
                    | CardMovement Location Location [Cards] -- ^ some cards moved
instance JSON DominionUpdate where
    showJSON (ShuffleEvent p) = pack [sj "s", sj p]
    showJSON (NewTurn p) = pack [sj "t", sj p]
    showJSON (CardMovement from to cs) = pack $ sj "c" : sj from : sj to : map sj cs
    readJSON (JSArray [JSString "s", p]) = ShuffleEvent <$> rj p
    readJSON (JSArray [JSString "t", p]) = NewTurn <$> rj p
    readJSON (JSArray (JSString "c":from:to:cs) = CardMovement <$> rj from <*> rj to <*> mapM rj cs
    readJSON json = noParse "DominionUpdate" json

data DominionStatus = DominionStatus [(PId, String)] [(Location, [Cards])]
instance JSON DominionStatus where
    showJSON (DominionStatus players cards) = pack [sj players, sj cards]
    readJSON (JSArray [players, cards]) = DominionStatus <$> rj players <*> rj cards
    readJSON json = noParse "DominionStatus" json

data Cards = Cards Int (Maybe String)
instance JSON Cards where
    showJSON (Cards count (Just name)) = pack [sj count, sj name]
    showJSON (Cards count Nothing) = pack [sj count]
    readJSON (JSArray [count, name]) = Cards <$> rj count <*> (Just <$> rj name)
    readJSON (JSArray [count]) = Cards <$> rj count <*> return Nothing
    readJSON json = noParse "Cards" json

-- Player locations: draw, (draw-bottom), hand, discard, duration, mats, aside?
-- Neutral locations: play, supply, trash
data Location = PlayerLocation PId String | NeutralLocation String
instance JSON Location where
    showJSON (PlayerLocation p loc) = pack [sj "p", sj p, sj loc]
    showJSON (NeutralLocation loc) = pack [sj "n", sj loc]
    readJSON (JSArray [JSString "p", p, loc]) = PlayerLocation <$> rj p <*> rj loc
    readJSON (JSArray [JSString "n", loc]) = NeutralLocation <$> rj loc
    readJSON json = noParse "Location" json

-- | Sent server-to-client to tell how many cards/actions to pick
data Range = Range Int Int -- lower/upper bounds
instance JSON Range where
    showJSON (Range min max) = pack [sj min, sj max]
    readJSON (JSArray [min,max]) = Range <$> rj min <*> rj max
    readJSON json = noParse "Range" json

-- might be nice to include the card reason here as well...?
data ServerQuestion = PlayCard String [String] -- ^ reason (action, reaction, treasure), choices
                    | MoveCard Range Location Location [String] -- ^ gain, discard, etc
                    | ChooseEffect Range [Effect] -- ^ i.e. pawn, pirate ship, torturer
instance JSON ServerQuestion where
    showJSON (PlayCard reason choices) = pack $ sj "p" : sj reason : map sj choices
    showJSON (MoveCard range from to choices) = pack $ sj "m" : sj range : sj from
                                                       : sj to : map sj choices 
    showJSON (ChooseEffect range choices) = pack $ sj "e" : sj range : map sj choices
    readJSON (JSArray (JSString "p" : reason : cs)) = PlayCard <$> rj reason <*> mapM rj cs 
    readJSON (JSArray (JSString "m" : rng : from : to : cs)) = MoveCard <$> rj rng
                                                               <*> rj from <*> rk to <*> mapM rj cs
    readJSON (JSArray (JSString "e" : rng : cs)) = PlayCard <$> rj rng <*> mapM rj cs 
    readJSON json = noParse "ServerQuestion" json

-- data Effect = PlusN Int String -- ^ i.e. actions, buys, coins, cards
--             | Loot | GainCard Location String | DiscardCards Int
-- instance JSON Effect

type Effect = String  -- for now...?
