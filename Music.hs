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
  Part(..)
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
data Music = Music [Part] deriving (Eq, Show)

