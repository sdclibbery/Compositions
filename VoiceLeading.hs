{-|
Module      : VoiceLeading
Description : Voice leading analysis for music
-}

module VoiceLeading (
  Rule(..),
  Result(..),
  tryAddNote
) where
import Structure
import Note
import Interval
import Notes

data Rule = PartRange deriving (Show, Eq)

data Result = Ok | Error Rule | Warning Rule deriving (Show, Eq)

tryAddNote :: Music -> PartName -> Event -> Result
tryAddNote m p e = partRange p e 

partRange :: PartName -> Event -> Result
partRange p (Play _ n) = checkRange n $ range p
    where
      checkRange n (lo, hi) = if n >= lo && n <= hi then Ok else Error PartRange
      range Treble = (Note C Nat 4, Note C Nat 6)
      range Alto   = (Note G Nat 3, Note F Nat 5)
      range Tenor  = (Note C Nat 3, Note C Nat 5)
      range Bass   = (Note E Nat 2, Note E Nat 4)
