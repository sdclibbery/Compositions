{-|
Module      : Diatonic.Interval
Description : Define and work with diatonic intervals
-}

module Diatonic.Interval where
import Diatonic.Keys
import Diatonic.Diatone
import qualified Euterpea.Music.Note.Music as Music

-- |Definition of a Diatonic interval
data Interval = MkInterval { intType :: Type, intDelta :: Delta } deriving (Eq, Ord, Show)

-- |Basic categorisations on intervals
data Type = Perfect | Major | Minor | Diminished | Augmented deriving (Eq, Ord, Show, Enum, Bounded)

-- |Actual diatonic difference of an interval
data Delta = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave | Ninth | Eleventh | Thirteenth deriving (Eq, Ord, Show, Enum, Bounded)

-- |Determine whether an interval definition represents a valid interval
isValid :: Interval -> Bool
isValid i = case (lookup i intervals) of
	Just _ -> True
	Nothing -> False

-- |Determine whether an interval is dissonant
isDissonant :: Interval -> Bool
isDissonant i = case i of
  MkInterval { intDelta = Second }     -> True
  MkInterval { intDelta = Seventh }    -> True
  MkInterval { intDelta = Eleventh }   -> True
  MkInterval { intDelta = Thirteenth } -> True
  MkInterval { intType = Diminished }  -> True
  MkInterval { intType = Augmented }   -> True
  otherwise                            -> False

-- |Determine whether an interval is consonant
isConsonant :: Interval -> Bool
isConsonant i = not $ isDissonant i

-- |Invert an interval
invert :: Interval -> Interval
invert (MkInterval t d) = MkInterval (invertType t) (invertDelta d)

-- |Given two diatonic pitches, get the interval between them
diatonicInterval :: Music.Mode -> Diatone -> Diatone -> Interval
diatonicInterval m dt1@(MkDiatone d1 o1) dt2@(MkDiatone d2 o2) = deltasToInterval $ MkInfo diatonicDelta chromaticDelta
	where
		diatonicDelta = abs $ (fromEnum d2) - (fromEnum d1) + 7*(o2 - o1)
		chromaticDelta = abs $ (diatoneToChromaticDelta m dt2) - (diatoneToChromaticDelta m dt1)

-- |Given two pitches, get the interval between them
pitchInterval :: Music.Pitch -> Music.Pitch -> Interval
pitchInterval (pc1, o1) (pc2, o2) = deltasToInterval $ MkInfo diatonicDelta chromaticDelta
	where
		diatonicDelta = abs $ pcToBaseInt pc2 - pcToBaseInt pc1 + 7*(o2 - o1)
		chromaticDelta = abs $ Music.absPitch (pc2,o2) - Music.absPitch (pc1,o1)
		pcToBaseInt pc = fromEnum $ pcToBasePc pc

-- |Get the interval representing the resolution of another, dissonant, interval
resolve :: Interval -> (Int, Interval)
resolve interval
	| interval == MkInterval Diminished Fifth = (1, MkInterval Major Third) -- Resolve 'inwards': up a diatone, then a Major Third
	-- Needs to handle more cases :-)
	| otherwise = error $ "Cannot resolve interval " ++ show interval ++ ": Its not dissonant (or not supported yet :-)"


-- Helpers

data Info = MkInfo { infoDiatonic :: Int, infoChromatic :: Int } deriving (Eq, Ord, Show)

intervals :: [(Interval, Info)]
intervals = [
	( MkInterval Perfect Unison,		MkInfo 0 0  ),
	( MkInterval Augmented Unison,		MkInfo 0 1  ),
	( MkInterval Minor Second,			MkInfo 1 1  ),
	( MkInterval Major Second,			MkInfo 1 2  ),
	( MkInterval Augmented Second,		MkInfo 1 3  ),
	( MkInterval Diminished Third,		MkInfo 2 2  ),
	( MkInterval Minor Third,			MkInfo 2 3  ),
	( MkInterval Major Third,			MkInfo 2 4  ),
	( MkInterval Diminished Fourth,		MkInfo 3 4  ),
	( MkInterval Perfect Fourth,		MkInfo 3 5  ),
	( MkInterval Augmented Fourth,		MkInfo 3 6  ),
	( MkInterval Diminished Fifth,		MkInfo 4 6  ),
	( MkInterval Perfect Fifth,			MkInfo 4 7  ),
	( MkInterval Augmented Fifth,		MkInfo 4 8  ),
	( MkInterval Minor Sixth,			MkInfo 5 8  ),
	( MkInterval Major Sixth,			MkInfo 5 9  ),
	( MkInterval Augmented Sixth,		MkInfo 5 10 ),
	( MkInterval Diminished Seventh,	MkInfo 6 9  ),
	( MkInterval Minor Seventh, 		MkInfo 6 10 ),
	( MkInterval Major Seventh,			MkInfo 6 11 ),
	( MkInterval Diminished Octave,		MkInfo 7 11 ),
	( MkInterval Perfect Octave,		MkInfo 7 12 ),
	( MkInterval Minor Ninth,			MkInfo 8 13 ),
	( MkInterval Major Ninth,			MkInfo 8 14 ),
	( MkInterval Augmented Ninth,		MkInfo 8 15 ),
	( MkInterval Diminished Eleventh,	MkInfo 10 16 ),
	( MkInterval Perfect Eleventh,		MkInfo 10 17 ),
	( MkInterval Augmented Eleventh,	MkInfo 10 18 ),
	( MkInterval Minor Thirteenth,		MkInfo 12 20 ),
	( MkInterval Major Thirteenth,		MkInfo 12 21 ),
	( MkInterval Augmented Thirteenth,	MkInfo 12 22 )
	]

invertType :: Type -> Type
invertType Perfect = Perfect
invertType Minor = Major
invertType Major = Minor
invertType Augmented = Diminished
invertType Diminished = Augmented

invertDelta :: Delta -> Delta
invertDelta Unison = Octave
invertDelta Second = Seventh
invertDelta Third = Sixth
invertDelta Fourth = Fifth
invertDelta Fifth = Fourth
invertDelta Sixth = Third
invertDelta Seventh = Second
invertDelta Octave = Unison
invertDelta d = error $ "Cannot invert a " ++ show d

deltasToInterval :: Info -> Interval
deltasToInterval ii = case (filter ((== ii).snd) intervals) of
	[(i,_)] -> i
	[] -> deltasToInterval $ MkInfo (infoDiatonic ii `mod` 7) (infoChromatic ii `mod` 12)

