-- Interval based music: easy to transpose within a key

{- ToDo:
* Try switching from enum to Int for Delta type...
* pitchInterval..? Would need a keysigf too..??
-> Then look at implementing analysis for diatonic laws of melody and harmony
-}

module Diatonic.Interval where
import Diatonic.Diatone
import qualified Euterpea.Music.Note.Music as Music

data Interval = MkInterval { intType :: Type, intDelta :: Delta } deriving (Eq, Ord, Show)

data Delta = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave | Ninth | Eleventh | Thirteenth deriving (Eq, Ord, Show, Enum, Bounded)
data Type = Perfect | Major | Minor | Diminished | Augmented deriving (Eq, Ord, Show, Enum, Bounded)
data Info = MkInfo { infoChromatic :: Int, infoDiatonic :: Int } deriving (Eq, Ord, Show)

intervals :: [(Interval, Info)]
intervals = [
	( MkInterval Perfect Unison, MkInfo 0 0  ),
	( MkInterval Augmented Unison, MkInfo 0 1  ),
	( MkInterval Minor Second, MkInfo 1 1  ),
	( MkInterval Major Second, MkInfo 1 2  ),
	( MkInterval Augmented Second, MkInfo 1 3  ),
	( MkInterval Diminished Third, MkInfo 2 2  ),
	( MkInterval Minor Third, MkInfo 2 3  ),
	( MkInterval Major Third, MkInfo 2 4  ),
	( MkInterval Diminished Fourth, MkInfo 3 4  ),
	( MkInterval Perfect Fourth, MkInfo 3 5  ),
	( MkInterval Augmented Fourth, MkInfo 3 6  ),
	( MkInterval Diminished Fifth, MkInfo 4 6  ),
	( MkInterval Perfect Fifth, MkInfo 4 7  ),
	( MkInterval Augmented Fifth, MkInfo 4 8  ),
	( MkInterval Minor Sixth, MkInfo 5 8  ),
	( MkInterval Major Sixth, MkInfo 5 9  ),
	( MkInterval Augmented Sixth, MkInfo 5 10  ),
	( MkInterval Diminished Seventh, MkInfo 6 9 ),
	( MkInterval Minor Seventh, MkInfo 6 10  ),
	( MkInterval Major Seventh, MkInfo 6 11 ),
	( MkInterval Diminished Octave, MkInfo 7 11 ),
	( MkInterval Perfect Octave, MkInfo 7 12 ),
	( MkInterval Minor Ninth, MkInfo 8 13 ),
	( MkInterval Major Ninth, MkInfo 8 14 ),
	( MkInterval Augmented Ninth, MkInfo 8 15 ),
	( MkInterval Diminished Eleventh, MkInfo 10 16 ),
	( MkInterval Perfect Eleventh, MkInfo 10 17 ),
	( MkInterval Augmented Eleventh, MkInfo 10 18 ),
	( MkInterval Minor Thirteenth, MkInfo 12 20 ),
	( MkInterval Major Thirteenth, MkInfo 12 21 ),
	( MkInterval Augmented Thirteenth, MkInfo 12 22 )
	]

isValid :: Interval -> Bool
isValid i = case (lookup i intervals) of
	Just _ -> True
	Nothing -> False

isDissonant :: Interval -> Bool
isDissonant i = case i of
  MkInterval { intDelta = Second }     -> True
  MkInterval { intDelta = Seventh }    -> True
  MkInterval { intDelta = Eleventh }   -> True
  MkInterval { intDelta = Thirteenth } -> True
  MkInterval { intType = Diminished }  -> True
  MkInterval { intType = Augmented }   -> True
  otherwise                            -> False

isConsonant :: Interval -> Bool
isConsonant i = not $ isDissonant i

-- This can probably be made a lot cleaner :-/
deltasToInterval :: Info -> Interval
deltasToInterval ii = case (filter ((== ii).snd) intervals) of
	[(i,_)] -> i
	[] -> deltasToInterval $ MkInfo ((infoChromatic ii) `mod` 7) ((infoDiatonic ii) `mod` 12)

diatonicInterval :: Music.Mode -> Diatone -> Diatone -> Interval
diatonicInterval m (d1,o1) (d2,o2) = deltasToInterval $ MkInfo diatonicDelta chromaticDelta
	where
		diatonicDelta = abs $ (fromEnum d2) - (fromEnum d1) + 7*(o2 - o1)
		chromaticDelta = abs $ (diatoneToAbsPitch m d2) - (diatoneToAbsPitch m d1) + 12*(o2 - o1)

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

invert :: Interval -> Interval
invert (MkInterval t d) = MkInterval (invertType t) (invertDelta d)

--pitchInterval :: Pitch -> Pitch -> Interval
