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
tryAddNote m p e = Ok


