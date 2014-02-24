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

type Diatone = (Degree, Octave)

i,ii,iii,iv,v,vi,vii :: Octave -> Dur -> Music Diatone
i octave dur = note dur $ (Tonic, octave)
ii octave dur = note dur $ (SuperTonic, octave)
iii octave dur = note dur $ (Mediant, octave)
iv octave dur = note dur $ (SubDominant, octave)
v octave dur = note dur $ (Dominant, octave)
vi octave dur = note dur $ (SubMediant, octave)
vii octave dur = note dur $ (LeadingNote, octave)

diatoneToChromaticDelta :: Mode -> Diatone -> AbsPitch
diatoneToChromaticDelta Major (Tonic, o) = 0 + o*12
diatoneToChromaticDelta Major (SuperTonic, o) = 2 + o*12
diatoneToChromaticDelta Major (Mediant, o) = 4 + o*12
diatoneToChromaticDelta Major (SubDominant, o) = 5 + o*12
diatoneToChromaticDelta Major (Dominant, o) = 7 + o*12
diatoneToChromaticDelta Major (SubMediant, o) = 9 + o*12
diatoneToChromaticDelta Major (LeadingNote, o) = 11 + o*12
diatoneToChromaticDelta Minor (Tonic, o) = 0 + o*12
diatoneToChromaticDelta Minor (SuperTonic, o) = 2 + o*12
diatoneToChromaticDelta Minor (Mediant, o) = 3 + o*12
diatoneToChromaticDelta Minor (SubDominant, o) = 5 + o*12
diatoneToChromaticDelta Minor (Dominant, o) = 7 + o*12
diatoneToChromaticDelta Minor (SubMediant, o) = 8 + o*12
diatoneToChromaticDelta Minor (LeadingNote, o) = 10 + o*12

diatoneToPitch :: PitchClass -> Mode -> Octave -> Diatone -> Pitch
diatoneToPitch pc mode baseOct (deg, octave) = pitch $ (pcToInt pc) + (baseOct+octave)*12 + (diatoneToChromaticDelta mode (deg, 0))

mDiatoneToPitch :: PitchClass -> Mode -> Octave -> Music Diatone -> Music Pitch
mDiatoneToPitch pc mode octave m = mMap (diatoneToPitch pc mode octave) m

transposeDiatone :: Int -> Diatone -> Diatone
transposeDiatone offset (deg, octave) = (toEnum degM, octave + octaveO)
	where
		degZ = fromEnum(deg) + offset
		degM = degZ `mod` 7
		octaveO = degZ `div` 7

diatonicTranspose :: Music Diatone -> Int -> Music Diatone
diatonicTranspose m offset = mMap (transposeDiatone offset) m 
