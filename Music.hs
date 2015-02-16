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

-- |Part name
data Part = Bass | Tenor | Alto | Soprano deriving (Eq, Show)

-- |One note or rest in a part
data SeqEvent = Rest Duration | Play Duration Note deriving (Eq, Show)

-- |List of notes and rests in sequence
data SeqPart = SeqPart { name :: Part, events :: [SeqEvent] } deriving (Eq, Show)

-- |Entire music made up of a list of parts in order from bass to treble
data Music = Music { bass :: SeqPart, tenor :: SeqPart, alto :: SeqPart, soprano :: SeqPart } deriving (Eq, Show)


{-
data Ctx = Ctx { start :: Time, end :: Time, part :: Part, prev :: Event, next :: Event, lower :: Event, higher :: Event } deriving (Eq, Show)

data Event = Rest Ctx | Play Ctx Note deriving (Eq, Show)
-}


-- |Empty music with empty parts
emptyMusic :: Music
emptyMusic = Music (empty Bass) (empty Tenor) (empty Alto) (empty Soprano)
  where
    empty p = SeqPart p []

-- |Get a list with all the inhabited parts
getParts :: Music -> [SeqPart]
getParts m = filter (not . null . events) [bass m, tenor m, alto m, soprano m]

-- |Add a new event to the end of a Part in some Music
addEvent :: Music -> Part -> SeqEvent -> Music
addEvent m pn e = replacePart m pn $ addToPart e $ findPart m pn
  where
    findPart (Music b _ _ _) Bass = b
    findPart (Music _ t _ _) Tenor = t
    findPart (Music _ _ a _) Alto = a
    findPart (Music _ _ _ s) Soprano = s
    addToPart e (SeqPart n es) = (SeqPart n $ es++[e])
    replacePart (Music b t a s) Bass p = Music p t a s
    replacePart (Music b t a s) Tenor p = Music b p a s
    replacePart (Music b t a s) Alto p = Music b t p s
    replacePart (Music b t a s) Soprano p = Music b t a p
