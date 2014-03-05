-- Music based around degrees of a diatonic scale, allowing diatonic transposition

{-
TODO:
* Minor scale diatoneToPitch is not right because it only converts via AbsPitch and that is wrong :-/ See commented test...
* Should really have mPitchToDiatone here too to complement mDiatoneToPitch :-)
-}

module Diatonic.Diatone where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic

data Degree = Tonic | SuperTonic | Mediant | SubDominant | Dominant | SubMediant | LeadingNote deriving (Eq, Ord, Show, Enum)

data Diatone = MkDiatone {diaDegree :: Degree, diaOctave :: Octave} deriving (Eq, Show)

instance Ord Diatone where
	l `compare` r = diatoneToChromaticDelta Major l `compare` diatoneToChromaticDelta Major r

dti,dtii,dtiii,dtiv,dtv,dtvi,dtvii :: Octave -> Diatone
dti octave = MkDiatone Tonic octave
dtii octave = MkDiatone SuperTonic octave
dtiii octave = MkDiatone Mediant octave
dtiv octave = MkDiatone SubDominant octave
dtv octave = MkDiatone Dominant octave
dtvi octave = MkDiatone SubMediant octave
dtvii octave = MkDiatone LeadingNote octave

i,ii,iii,iv,v,vi,vii :: Octave -> Dur -> Music Diatone
i octave dur = note dur $ dti octave
ii octave dur = note dur $ dtii octave
iii octave dur = note dur $ dtiii octave
iv octave dur = note dur $ dtiv octave
v octave dur = note dur $ dtv octave
vi octave dur = note dur $ dtvi octave
vii octave dur = note dur $ dtvii octave

diatoneToChromaticDelta :: Mode -> Diatone -> AbsPitch
diatoneToChromaticDelta Major (MkDiatone Tonic o) = 0 + o*12
diatoneToChromaticDelta Major (MkDiatone SuperTonic o) = 2 + o*12
diatoneToChromaticDelta Major (MkDiatone Mediant o) = 4 + o*12
diatoneToChromaticDelta Major (MkDiatone SubDominant o) = 5 + o*12
diatoneToChromaticDelta Major (MkDiatone Dominant o) = 7 + o*12
diatoneToChromaticDelta Major (MkDiatone SubMediant o) = 9 + o*12
diatoneToChromaticDelta Major (MkDiatone LeadingNote o) = 11 + o*12
diatoneToChromaticDelta Minor (MkDiatone Tonic o) = 0 + o*12
diatoneToChromaticDelta Minor (MkDiatone SuperTonic o) = 2 + o*12
diatoneToChromaticDelta Minor (MkDiatone Mediant o) = 3 + o*12
diatoneToChromaticDelta Minor (MkDiatone SubDominant o) = 5 + o*12
diatoneToChromaticDelta Minor (MkDiatone Dominant o) = 7 + o*12
diatoneToChromaticDelta Minor (MkDiatone SubMediant o) = 8 + o*12
diatoneToChromaticDelta Minor (MkDiatone LeadingNote o) = 11 + o*12

diatoneToPitch :: PitchClass -> Mode -> Octave -> Diatone -> Pitch
diatoneToPitch pc mode baseOct (MkDiatone deg octave) = pitch $ (pcToInt pc) + (baseOct+octave)*12 + (diatoneToChromaticDelta mode (MkDiatone deg 0))

mDiatoneToPitch :: PitchClass -> Mode -> Octave -> Music Diatone -> Music Pitch
mDiatoneToPitch pc mode octave m = mMap (diatoneToPitch pc mode octave) m

transposeDiatone :: Int -> Diatone -> Diatone
transposeDiatone offset (MkDiatone deg octave) = (MkDiatone (toEnum degM) (octave + octaveO))
	where
		degZ = fromEnum(deg) + offset
		degM = degZ `mod` 7
		octaveO = degZ `div` 7

diatonicTranspose :: Music Diatone -> Int -> Music Diatone
diatonicTranspose m offset = mMap (transposeDiatone offset) m 
