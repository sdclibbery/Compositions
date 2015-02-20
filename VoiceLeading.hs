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

data Rule = PartRange | PartsCrossing deriving (Show, Eq)

data Result = Ok | Error Rule | Warning Rule deriving (Show, Eq)

tryAddNote :: Music -> Part -> SeqEvent -> Result
tryAddNote m p e = partRange p e 

partRange :: Part -> SeqEvent -> Result
partRange p (SeqPlay _ n) = checkRange n $ range p
    where
      checkRange n (lo, hi) = if n >= lo && n <= hi then Ok else Error PartRange
      range Soprano = (Note C Nat 4, Note C Nat 6)
      range Alto    = (Note G Nat 3, Note F Nat 5)
      range Tenor   = (Note C Nat 3, Note C Nat 5)
      range Bass    = (Note E Nat 2, Note E Nat 4)

-- !! So. Need to find the notes from the OTHER voices which this one might cross...
-- Suggest adding the following to Music:
--  partDuration :: Music -> Part
--  noteAt :: Music -> Part -> Time
partsCrossing :: Music -> Part -> SeqEvent -> Result
partsCrossing m p (SeqPlay _ n) = Ok
