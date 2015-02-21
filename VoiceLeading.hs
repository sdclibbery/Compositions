{-|
Module      : VoiceLeading
Description : Voice leading analysis for music
-}

module VoiceLeading (
  Rule(..),
  Result(..),
  tryAddNote
) where
import Music
import Note
import Interval
import Notes
import Data.Maybe

-- |Voice Leading rules
data Rule = PartRange | PartsCrossing deriving (Show, Eq)

-- |Result from voice leading analysis
data Result = Error Rule | Warning Rule deriving (Show, Eq)

-- |Try adding a note to the end of a Music part and see if any voice leading problems would result.
tryAddNote :: Music -> Part -> SeqEvent -> [Result]
tryAddNote m p e = catMaybes [
    partRange p e,
    partsCrossing m p e
  ]

partRange :: Part -> SeqEvent -> Maybe Result
partRange p (SeqPlay _ n) = checkRange n $ range p
    where
      checkRange n (lo, hi) = if n >= lo && n <= hi then Nothing else Just $ Error PartRange
      range Soprano = (Note C Nat 4, Note C Nat 6)
      range Alto    = (Note G Nat 3, Note F Nat 5)
      range Tenor   = (Note C Nat 3, Note C Nat 5)
      range Bass    = (Note E Nat 2, Note E Nat 4)

partsCrossing :: Music -> Part -> SeqEvent -> Maybe Result
partsCrossing m p (SeqPlay _ n) = Nothing
