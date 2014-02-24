-- Analysis of Melody: IE a sequential composition 

{-
TODO:
* Find a better way of getting timing: State monad?
 * Clean up the nasty (Dur, Diatone) type too, and get rid of the where in notesToIntervals...
* Better handling of rests: split into multiple melodic lines
* More rules :-)
-}

module Diatonic.Analysis.Melody where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Diatonic.Interval
import Diatonic.Diatone
import qualified Data.Maybe

data Result = Warning Dur String | Error Dur String deriving (Eq, Ord, Show)

type IntervalInfo = (Dur, Interval, Diatone, Diatone, Maybe Diatone)

-- Section 89 in Prousts Harmony
-- Any dissonance other than a second is bad
ruleS89 :: Mode -> IntervalInfo -> [Result]
ruleS89 mo (t, i, d1, d2, md3)
	| isConsonant(i)			= []
	| intDelta i == Second		= []
	| intType i == Diminished	= [] -- Leave for rule S90
	| intType i == Augmented	= [] -- Leave for rule S91
	| otherwise					= [Error t $ "Dissonance " ++ show (intType i) ++ show (intDelta i)]

-- Section 90 in Prousts Harmony
-- A diminished interval must be resolved correctly
ruleS90 :: Mode -> IntervalInfo -> [Result]
ruleS90 mo (t, i, d1, d2, md3)
	| intType i == Diminished	= evaluateDiminished d1 d2 md3
	| otherwise					= []
	where
		msg = "Dissonance " ++ show (intType i) ++ show (intDelta i)
		evaluateDiminished d1 d2 Nothing = [Warning t msg]
		evaluateDiminished d1 d2 (Just d3) =
			if not (isInInterval d1 d2 d3) then [Error t $ "Outside "  ++ msg]
			else
				if d3 == resolution mo d1 d2 then []
				else [Error t $ "Unresolved "  ++ msg]
		isInInterval d1 d2 d3 = inRange (diatoneToChromaticDelta mo d1) (diatoneToChromaticDelta mo d2) (diatoneToChromaticDelta mo d3)

-- Section 91 in Prousts Harmony
-- An augmented interval is always bad (except augmented second)
ruleS91 :: Mode -> IntervalInfo -> [Result]
ruleS91 mo (t, i, d1, d2, md3)
	| intType i == Augmented && intDelta i /= Second = [Error t $ "Dissonance " ++ show (intType i) ++ show (intDelta i)]
	| otherwise = []

inRange :: Ord a => a -> a -> a -> Bool
inRange l r n
	| l < r = l < n && r > n
	| l > r = l > n && r < n
	| l == r = l == n

resolution :: Mode -> Diatone -> Diatone -> Diatone
resolution mo d1 d2
 	| cd2 < cd1 = lowerResolution
	| cd2 > cd1 = upperResolution
	where
		cd1 = diatoneToChromaticDelta mo d1
		cd2 = diatoneToChromaticDelta mo d2
		(diatonicDelta, resolvedInterval) = resolve (diatonicInterval mo d1 d2)
		lowerResolution = transposeDiatone diatonicDelta lowerDiatone
		upperResolution = transposeDiatone (fromEnum $ intDelta resolvedInterval) lowerResolution
		lowerDiatone = if cd1 < cd2 then d1 else d2

notesToIntervals :: Mode -> [(Dur,Diatone)] -> [IntervalInfo]
notesToIntervals _ []			= []
notesToIntervals _ (d:[])		= []
notesToIntervals mo ((t1,d1):n2@(_,d2):ns)	= (t1, diatonicInterval mo d1 d2, d1, d2, followOn ns) : (notesToIntervals mo (n2:ns))
	where
		followOn [n] = Just $ snd n
		followOn [] = Nothing

melodyToNotes :: Dur -> Music Diatone -> [(Dur,Diatone)]
melodyToNotes t (Prim (Rest _)) = []
melodyToNotes t (Prim (Note _ d)) = [(t, d)]
melodyToNotes t (n :+: ns) = melodyToNotes t n ++ melodyToNotes (dur n + t) ns
melodyToNotes _ _ = error "melodyToNotes: invalid input: not just a sequential melody!"

analyseMelody :: Mode -> Music Diatone -> [Result]
analyseMelody mo m = foldl rules [] intervals
	where
		intervals = notesToIntervals mo $ melodyToNotes 0 m
		rules rs ii = rs
			++ ruleS89 mo ii
			++ ruleS90 mo ii
			++ ruleS91 mo ii
