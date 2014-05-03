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
import Debug.Trace

-- |Provide a harmonic analysis of some music
analyseMusic :: Mode -> [Sequence Diatone] -> [Result]
analyseMusic mo = foldr ((++) . analyseSequences mo) []
					. map (toIntervalInfos mo)
					. map correlateSequences
					. allPairs

-- Helpers

-- Section 96 in Prouts Harmony
-- Consequtive unisons are bad
ruleS96 :: Mode -> IntervalInfo -> [Result]
ruleS96 mo (_, _, _, _, (_, _, Exact)) = [] -- If the sequences double each other then the consecutive rules dont apply
ruleS96 mo (_, Repeat, _, _, _) = [] -- Repeated octaves or unsions are fine
ruleS96 mo (t, _, MkInterval _ Unison, MkInterval _ Unison, ctx) = [Error (parts ctx) t (Harmony 96) $ "Consecutive unisons"]
ruleS96 mo (t, m, MkInterval _ Octave, MkInterval _ Octave, ctx) = [Error (parts ctx) t (Harmony 96) $ "Consecutive octaves"]
ruleS96 _ _ = []

-- !! rule98 says that if the ENTIRE sequence overlap is in octaves or unsions, then its allowed...
-- ? Dont think we can check this in the rule because correlateSequences will have already stripped the 'tail', so we'll think its doubled even if one sequence has a 'tail'
-- So will need something else; either detect in correlateSequences or something else... :-/	

parts :: Context -> [Part]
parts ((p1,_,_), (p2,_,_), _) = [p1, p2]

-- Apply the rules to analyse the harmonic relation between two sequences

analyseSequences :: Mode -> [IntervalInfo] -> [Result]
analyseSequences mo = foldr rules []
	where
		rules ii rs = rs
			++ ruleS96 mo ii

data Motion = Repeat | Similar | Oblique | Contrary deriving (Show, Eq)

type PartContext = (Part, Diatone, Diatone)
type Context = (PartContext, PartContext, Fit)

type IntervalInfo = ( Time, Motion, Interval, Interval, Context )

toIntervalInfos :: Mode -> (Part, Part, [(Time, Diatone, Diatone)], Fit) -> [IntervalInfo]
toIntervalInfos _ (_, _, [], _) = []
toIntervalInfos _ (_, _, (_:[]), _) = []
toIntervalInfos mo (pa, pb, (t1, da1, db1):x@(t2, da2, db2):xs, o) = (t2, motion, i da1 db1, i da2 db2, ctx) : toIntervalInfos mo (pa, pb, (x:xs), o)
	where
		i = diatonicInterval mo
		ctx = ( (pa, da1, da2), (pb, db1, db2), o )
		motion = if cmpa == EQ && cmpb == EQ then Repeat else
					if cmpa == cmpb then Similar else
					if cmpa == EQ || cmpb == EQ then Oblique else trace ("\nContrary:"++show cmpa++" "++show cmpb++" "++show ctx) Contrary
			where
				cmpa = da1 `compare` da2
				cmpb = db1 `compare` db2

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs xxs@(x:xs) = zip (repeat x) xs ++ allPairs xs

