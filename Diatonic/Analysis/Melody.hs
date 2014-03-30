{-|
Module      : Diatonic.Analysis.Melody
Description : Provide harmonic and contrapuntal analysis of purely sequential (melodic) music

Provide analysis of purely sequential (melodic) music.
No analysis of note against note (ie harmonic or contrapuntal) is undertaken in this module.
The analytic rules is according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Diatonic.Analysis.Melody where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Diatonic.Interval
import Diatonic.Diatone
import Diatonic.Analysis.Deconstruct
import qualified Data.Maybe

-- |Define the source for an error or warning within Prouts books
data Source = Harmony Int | CounterPoint Int deriving (Eq, Ord, Show)

-- |One analysis result: an error or warning
data Result = Warning Time Source String | Error Time Source String deriving (Eq, Ord, Show)

-- |Provide a melodic analysis of some music
analyseMusic :: Mode -> Music Diatone -> [Result]
analyseMusic mo m = foldr ((++) . analyseSequence mo) [] $ musicToSequences m

-- Helpers

-- Section 89 in Prouts Harmony
-- Any dissonance other than a second is bad
ruleS89 :: Mode -> IntervalInfo -> [Result]
ruleS89 mo (t, i, md0, d1, d2, md3)
	| isConsonant(i)			= []
	| intDelta i == Second		= []
	| intType i == Diminished	= [] -- Leave for rule S90
	| intType i == Augmented	= [] -- Leave for rule S91
	| otherwise					= [Error t (Harmony 89) $ "Dissonance " ++ show (intType i) ++ show (intDelta i)]

-- Section 90 in Prouts Harmony
-- A diminished interval must be resolved correctly
ruleS90 :: Mode -> IntervalInfo -> [Result]
ruleS90 mo (t, i, md0, d1, d2, md3)
	| intType i == Diminished	= evaluateDiminished d1 d2 md3
	| otherwise					= []
	where
		msg = "Dissonance " ++ show (intType i) ++ show (intDelta i)
		evaluateDiminished d1 d2 Nothing = [Warning t (Harmony 90) msg]
		evaluateDiminished d1 d2 (Just d3) =
			if not (isInInterval d1 d2 d3) then [Error t (Harmony 90) $ "Outside "  ++ msg]
			else
				if d3 == resolution mo d1 d2 then []
				else [Error t (Harmony 90) $ "Unresolved "  ++ msg]
		isInInterval d1 d2 d3 = d3 > min d1 d2 && d3 < max d1 d2

-- Section 91 in Prouts Harmony
-- An augmented interval is always bad (except augmented second)
ruleS91 :: Mode -> IntervalInfo -> [Result]
ruleS91 mo (t, i, md0, d1, d2, md3)
	| intType i == Augmented && intDelta i /= Second = [Error t (Harmony 91) $ "Dissonance " ++ show (intType i) ++ show (intDelta i)]
	| otherwise = []

-- Section 92 in Prouts Harmony
-- A large interval must be approached and left in the opposite direction to the interval
ruleS92 :: Mode -> IntervalInfo -> [Result]
ruleS92 mo (t, i, md0, d1, d2, md3) = approach md0 ++ leave md3
	where
		approach Nothing = []
		approach (Just d0) = if isLarge && sameDirection d0 d1 d2 then [Error t (Harmony 92) $ "Large Interval Approach"] else []
		leave Nothing = []
		leave (Just d3) = if isLarge && sameDirection d1 d2 d3 then [Error t (Harmony 92) $ "Large Interval Leave"] else []
		isLarge = intDelta i > Sixth
		sameDirection s1 s2 s3
			| s1 < s2 = s2 < s3
			| s1 > s2 = s2 > s3
			| otherwise = s3 == s2

-- Rule helpers

resolution :: Mode -> Diatone -> Diatone -> Diatone
resolution mo d1 d2
 	| d2 < d1 = lowerResolution
	| d2 > d1 = upperResolution
	where
		(diatonicDelta, resolvedInterval) = resolve (diatonicInterval mo d1 d2)
		lowerResolution = transposeDiatone diatonicDelta lowerDiatone
		upperResolution = transposeDiatone (fromEnum $ intDelta resolvedInterval) lowerResolution
		lowerDiatone = min d1 d2

-- Apply the rules to analyse a single melody 

analyseSequence :: Mode -> Sequence Diatone -> [Result]
analyseSequence mo seq = foldr rules [] $ notesToIntervals mo seq
	where
		rules ii rs = rs
			++ ruleS89 mo ii
			++ ruleS90 mo ii
			++ ruleS91 mo ii
			++ ruleS92 mo ii


-- Helpers to turn sequences into lists that show intervals suitable for rule apllication

type IntervalInfo = (Time, Interval, Maybe Diatone, Diatone, Diatone, Maybe Diatone)
type DtEvent = Event Diatone

toIntervalInfo :: Mode -> Time -> [(Maybe DtEvent, DtEvent, DtEvent, Maybe DtEvent)] -> [IntervalInfo]
toIntervalInfo _ _ [] = []
toIntervalInfo mo t ((mn0, (t1,d1), (_,d2), mn3):xs) = (t+t1, i, fmap snd mn0, d1, d2, fmap snd mn3) : toIntervalInfo mo (t+t1) xs
	where
		i = diatonicInterval mo d1 d2

listExpand :: [a] -> [(Maybe a, a, a, Maybe a)]
listExpand [] = []
listExpand (x:[]) = []
listExpand (x1:x2:xs) = (Nothing, x1, x2, Data.Maybe.listToMaybe xs) : next x1 x2 xs
	where
		next x0 x1 [] = []
		next x0 x1 (x2:[]) = [(Just x0, x1, x2, Nothing)]
		next x0 x1 (x2:x3:xs) = (Just x0, x1, x2, Just x3) : next x1 x2 xs

notesToIntervals :: Mode -> Sequence Diatone -> [IntervalInfo]
notesToIntervals mo (Temporal t evts) = toIntervalInfo mo t $ listExpand evts
