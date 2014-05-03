{-|
Module      : Diatonic.Diatone
Description : Music based around degrees of a diatonic scale, allowing diatonic transposition
-}

{-
TODO:
* Should really have mPitchToDiatone here too to complement mDiatoneToPitch :-)
-}

module Diatonic.Diatone where
import Diatonic.Keys
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic

-- |Degree of a diatonic scale
data Degree = Tonic | SuperTonic | Mediant | SubDominant | Dominant | SubMediant | LeadingNote deriving (Eq, Ord, Show, Enum)

-- |Definition of a diatonic pitch value: independent of key
data Diatone = MkDiatone {diaDegree :: Degree, diaOctave :: Octave} deriving (Eq, Show)

-- |Allow comparison of two Diatones based on 'absolute' pitch. Naturally assumes that both diatones are in the same key.
instance Ord Diatone where
	l `compare` r = diatoneToChromaticDelta Major l `compare` diatoneToChromaticDelta Major r

-- |Quick sugar functions for creating Diatones
dti,dtii,dtiii,dtiv,dtv,dtvi,dtvii :: Octave -> Diatone
dti octave = MkDiatone Tonic octave
dtii octave = MkDiatone SuperTonic octave
dtiii octave = MkDiatone Mediant octave
dtiv octave = MkDiatone SubDominant octave
dtv octave = MkDiatone Dominant octave
dtvi octave = MkDiatone SubMediant octave
dtvii octave = MkDiatone LeadingNote octave

-- |Quick sugar functions for creating musical notes based on diatones
i,ii,iii,iv,v,vi,vii :: Octave -> Dur -> Music Diatone
i octave dur = note dur $ dti octave
ii octave dur = note dur $ dtii octave
iii octave dur = note dur $ dtiii octave
iv octave dur = note dur $ dtiv octave
v octave dur = note dur $ dtv octave
vi octave dur = note dur $ dtvi octave
vii octave dur = note dur $ dtvii octave

-- |Get the absolute pitch value when a Diatone is placed into a given key
diatoneToAbsPitch :: PitchClass -> Mode -> Diatone -> AbsPitch
diatoneToAbsPitch pc mode (MkDiatone deg octave) = (pcToInt pc) + octave*12 + (diatoneToChromaticDelta mode (MkDiatone deg 0))

-- |Get the pitch value when a Diatone is placed into a given key
diatoneToPitch :: PitchClass -> Mode -> Diatone -> Pitch
diatoneToPitch pc mode (MkDiatone d o) = (pitchInKey pc mode (fromEnum d), o + extraO)
  where
    baseDegreeOfScale = pcToBasePc pc
    extraO = if fromEnum baseDegreeOfScale + fromEnum d > 6 then 1 else 0

-- |Convert diatonic music to pitch music by placing it into a key signature
mDiatoneToPitch :: PitchClass -> Mode -> Music Diatone -> Music Pitch
mDiatoneToPitch pc mode m = mMap (diatoneToPitch pc mode) m

-- |Transpose a Diatone (independent of key, but transposing within a key)
transposeDiatone :: Int -> Diatone -> Diatone
transposeDiatone offset (MkDiatone deg octave) = (MkDiatone (toEnum degM) (octave + octaveO))
	where
		degZ = fromEnum(deg) + offset
		degM = degZ `mod` 7
		octaveO = degZ `div` 7

-- |Transpose some diatonic music music within the same key
mTransposeDiatonic :: Music Diatone -> Int -> Music Diatone
mTransposeDiatonic m offset = mMap (transposeDiatone offset) m 

-- Helpers

diatoneToChromaticDelta :: Mode -> Diatone -> AbsPitch
diatoneToChromaticDelta Major (MkDiatone Tonic o)       = 0 + o*12
diatoneToChromaticDelta Major (MkDiatone SuperTonic o)  = 2 + o*12
diatoneToChromaticDelta Major (MkDiatone Mediant o)     = 4 + o*12
diatoneToChromaticDelta Major (MkDiatone SubDominant o) = 5 + o*12
diatoneToChromaticDelta Major (MkDiatone Dominant o)    = 7 + o*12
diatoneToChromaticDelta Major (MkDiatone SubMediant o)  = 9 + o*12
diatoneToChromaticDelta Major (MkDiatone LeadingNote o) = 11 + o*12
diatoneToChromaticDelta Minor (MkDiatone Tonic o)       = 0 + o*12
diatoneToChromaticDelta Minor (MkDiatone SuperTonic o)  = 2 + o*12
diatoneToChromaticDelta Minor (MkDiatone Mediant o)     = 3 + o*12
diatoneToChromaticDelta Minor (MkDiatone SubDominant o) = 5 + o*12
diatoneToChromaticDelta Minor (MkDiatone Dominant o)    = 7 + o*12
diatoneToChromaticDelta Minor (MkDiatone SubMediant o)  = 8 + o*12
diatoneToChromaticDelta Minor (MkDiatone LeadingNote o) = 11 + o*12
