{-|
Module      : Music
Description : Definition of musical structure for use in harmonic analysis
-}

module Music (
  Music(..),
  Time(..),
  Duration(..),
  PartName(..),
  SeqEvent(..),
  Part(..),
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
data PartName = Bass | Tenor | Alto | Soprano deriving (Eq, Show)

-- |One note or rest in a part
data SeqEvent = Rest Duration | Play Duration Note deriving (Eq, Show)

-- |List of notes and rests in sequence
data Part = Part { name :: PartName, events :: [SeqEvent] } deriving (Eq, Show)

-- |Entire music made up of a list of parts in order from bass to treble
data Music = Music { bass :: Part, tenor :: Part, alto :: Part, soprano :: Part } deriving (Eq, Show)

-- |Get a list with all the inhabited parts
getParts :: Music -> [Part]
getParts m = filter (not . null . events) [bass m, tenor m, alto m, soprano m]

-- |Add a new event to the end of a Part in some Music
addEvent :: Music -> PartName -> SeqEvent -> Music
addEvent m pn e = replacePart m pn $ addToPart e $ findPart m pn
  where
    findPart (Music b _ _ _) Bass = b
    findPart (Music _ t _ _) Tenor = t
    findPart (Music _ _ a _) Alto = a
    findPart (Music _ _ _ s) Soprano = s
    addToPart e (Part n es) = (Part n $ es++[e])
    replacePart (Music b t a s) Bass p = Music p t a s
    replacePart (Music b t a s) Tenor p = Music b p a s
    replacePart (Music b t a s) Alto p = Music b t p s
    replacePart (Music b t a s) Soprano p = Music b t a p
