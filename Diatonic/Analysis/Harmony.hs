{-|
Module      : Diatonic.Analysis.Harmony
Description : Provide harmonic analysis of Music

The analytic rules are according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Diatonic.Analysis.Harmony where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Diatonic.Interval
import Diatonic.Diatone
import Diatonic.Analysis.Deconstruct
import Diatonic.Analysis.Report

-- |Provide a harmonic analysis of some music
analyseMusic :: Mode -> Music Diatone -> [Result]
analyseMusic mo = foldr ((++) . analyseSequences mo) []
					. map (toIntervalInfos mo)
					. map correlateSequences
					. allPairs
					. musicToSequences

-- Helpers

-- Section 96 in Prouts Harmony
-- Consequtive unisons are bad
ruleS96 :: Mode -> IntervalInfo -> [Result]
ruleS96 mo (t, Repeat, _, _) = [] -- Repeated octaves or unsions are fine
ruleS96 mo (t, m, MkInterval _ Unison, MkInterval _ Unison) = [Error t (Harmony 96) $ "Consecutive unisons"]
ruleS96 mo (t, m, MkInterval _ Octave, MkInterval _ Octave) = [Error t (Harmony 96) $ "Consecutive octaves"]
ruleS96 mo (t, m, i0, i1) = []

-- !! rule98 says that if the ENTIRE sequence overlap is in octaves or unsions, then its allowed...

-- Apply the rules to analyse the harmonic relation between two sequences

analyseSequences :: Mode -> [IntervalInfo] -> [Result]
analyseSequences mo = foldr rules []
	where
		rules ii rs = rs
			++ ruleS96 mo ii

data Motion = Repeat | Similar | Oblique | Contrary deriving (Show, Eq)

type IntervalInfo = ( Time, Motion, Interval, Interval )

toIntervalInfos :: Mode -> [(Time, Diatone, Diatone)] -> [IntervalInfo]
toIntervalInfos _ [] = []
toIntervalInfos _ (_:[]) = []
toIntervalInfos mo ((t1, da1, db1):x@(t2, da2, db2):xs) = (t2, motion, i da1 db1, i da2 db2) : toIntervalInfos mo (x:xs)
	where
		i = diatonicInterval mo
		motion = if cmpa == EQ && cmpb == EQ then Repeat else
					if cmpa == cmpb then Similar else
					if cmpa == EQ || cmpb == EQ then Oblique else Contrary
			where
				cmpa = da1 `compare` da2
				cmpb = db1 `compare` db2

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs xxs@(x:xs) = zip (repeat x) xs ++ allPairs xs

