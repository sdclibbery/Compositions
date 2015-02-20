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


-- |One note or rest in a part
data SeqEvent = SeqRest Duration | SeqPlay Duration Note deriving (Eq, Show)

-- |List of notes and rests in sequence
data SeqPart = SeqPart { name :: Part, events :: [SeqEvent] } deriving (Eq, Show)


-- |Time type
type Time = Rational

-- |Duration type
type Duration = Rational

-- |Part
data Part = Bass | Tenor | Alto | Soprano deriving (Eq, Show)

-- |Context for a music event
data Ctx = Ctx { start :: Time, end :: Time, dur :: Duration, part :: Part, prev :: (Maybe Event), next :: (Maybe Event), lower :: (Maybe Event), higher :: (Maybe Event) } deriving (Eq, Show)

-- |One music event; note or rest, and the surrounding musical context
data Event = Rest Ctx | Play Ctx Note deriving (Eq, Show)

-- |Entire music made up of a list of parts in order from bass to treble
data Music = Music { bass :: [Event], tenor :: [Event], alto :: [Event], soprano :: [Event] } deriving (Eq, Show)


-- |Empty music with empty parts
emptyMusic :: Music
emptyMusic = Music [] [] [] []

-- |Get a list with all the inhabited parts
getParts :: Music -> [[Event]]
getParts m = filter (not . null) [bass m, tenor m, alto m, soprano m]

-- |Add a new event to the end of a Part in some Music
addEvent :: Music -> Part -> SeqEvent -> Music
addEvent m pn se = replacePart m pn $ addToPart se $ findPart m pn
  where
    findPart (Music b _ _ _) Bass = b
    findPart (Music _ t _ _) Tenor = t
    findPart (Music _ _ a _) Alto = a
    findPart (Music _ _ _ s) Soprano = s
    addToPart se es = es++[toEvent es se]
    toEvent es (SeqRest d) = Rest (makeEventCtx es d)
    toEvent es (SeqPlay d n) = Play (makeEventCtx es d) n
    replacePart (Music b t a s) Bass p = Music p t a s
    replacePart (Music b t a s) Tenor p = Music b p a s
    replacePart (Music b t a s) Alto p = Music b t p s
    replacePart (Music b t a s) Soprano p = Music b t a p

ctx :: Event -> Ctx
ctx (Rest c) = c
ctx (Play c _) = c

makeEventCtx :: [Event] -> Duration -> Ctx
makeEventCtx es d = Ctx _start _end _dur _part _prev _next _higher _lower
  where
    _start = if null es then 0 else end $ ctx $ last es
    _end = _start + d
    _dur = _end - _start
    _part = part $ ctx $ head es
    _prev = Just $ last es
    _next = Nothing
    _higher = Nothing -- !!! TODO!!!
    _lower = Nothing -- !!! TODO!!!
    -- !!! Need to link in to all neighbours as well!!
