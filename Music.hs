{-|
Module      : Music
Description : Definition of musical structure for use in harmonic analysis
-}

module Music (
  Music(..),
  Time(..),
  Duration(..),
  Part(..),
  SeqEvent(..),
  SeqPart(..),
  Ctx(..),
  Event(..),
  emptyMusic,
  getParts,
  addEvent
) where
import Note
import Data.List
import Data.Ord


-- |Time type
type Time = Rational

-- |Duration type
type Duration = Rational

-- |Part
data Part = Bass | Tenor | Alto | Soprano deriving (Eq, Show)

-- |Context for a music event
data Ctx = Ctx { start :: Time, end :: Time, dur :: Duration, part :: Part, prev :: [Event], next :: [Event], lower :: (Maybe Part), higher :: (Maybe Part) } deriving (Eq, Show)

-- |One music event; note or rest, and the surrounding musical context
data Event = Rest Ctx | Play Ctx Note deriving (Eq, Show)

-- |Entire music made up of a list of parts in order from bass to treble
data Music = Music { bass :: [Event], tenor :: [Event], alto :: [Event], soprano :: [Event] } deriving (Eq, Show)


-- |One note or rest in a sequence (used for constructing music)
data SeqEvent = SeqRest Duration | SeqPlay Duration Note deriving (Eq, Show)

-- |List of notes and rests in sequence (used for constructing music)
data SeqPart = SeqPart { name :: Part, events :: [SeqEvent] } deriving (Eq, Show)


-- |Empty music with empty parts
emptyMusic :: Music
emptyMusic = Music [] [] [] []

-- |Get a list with all the inhabited parts
getParts :: Music -> [[Event]]
getParts m = filter (not . null) [bass m, tenor m, alto m, soprano m]

-- |Add a new event to the end of a Part in some Music
addEvent :: Music -> Part -> SeqEvent -> Music
addEvent m p se = setPart m p $ addToPart se $ getPart m p
  where
    addToPart se es = es++[toEvent es se]
    toEvent es (SeqRest d) = Rest (makeEventCtx es p d)
    toEvent es (SeqPlay d n) = Play (makeEventCtx es p d) n
    -- !!! Need to link in to all neighbours as well!!

getPart :: Music -> Part -> [Event]
getPart (Music b _ _ _) Bass = b
getPart (Music _ t _ _) Tenor = t
getPart (Music _ _ a _) Alto = a
getPart (Music _ _ _ s) Soprano = s

setPart :: Music -> Part -> [Event] -> Music
setPart (Music _ t a s) Bass p = Music p t a s
setPart (Music b _ a s) Tenor p = Music b p a s
setPart (Music b t _ s) Alto p = Music b t p s
setPart (Music b t a _) Soprano p = Music b t a p

ctx :: Event -> Ctx
ctx (Rest c) = c
ctx (Play c _) = c

makeEventCtx :: [Event] -> Part -> Duration -> Ctx
makeEventCtx es p d = Ctx _start _end _dur _part _prev _next _higher _lower
  where
    _start = if null es then 0 else end $ ctx $ last es
    _end = _start + d
    _dur = _end - _start
    _part = p
    _prev = reverse es
    _next = []
    _higher = higherPart p
    _lower = lowerPart p

higherPart :: Part -> Maybe Part
higherPart Bass = Just Tenor
higherPart Tenor = Just Alto
higherPart Alto = Just Soprano
higherPart Soprano = Nothing

lowerPart :: Part -> Maybe Part
lowerPart Bass = Nothing
lowerPart Tenor = Just Bass
lowerPart Alto = Just Tenor
lowerPart Soprano = Just Alto