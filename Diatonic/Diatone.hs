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

diatoneToAbsPitch :: Mode -> Degree -> AbsPitch
diatoneToAbsPitch Major Tonic = 0
diatoneToAbsPitch Major SuperTonic = 2
diatoneToAbsPitch Major Mediant = 4
diatoneToAbsPitch Major SubDominant = 5
diatoneToAbsPitch Major Dominant = 7
diatoneToAbsPitch Major SubMediant = 9
diatoneToAbsPitch Major LeadingNote = 11
diatoneToAbsPitch Minor Tonic = 0
diatoneToAbsPitch Minor SuperTonic = 2
diatoneToAbsPitch Minor Mediant = 3
diatoneToAbsPitch Minor SubDominant = 5
diatoneToAbsPitch Minor Dominant = 7
diatoneToAbsPitch Minor SubMediant = 8
diatoneToAbsPitch Minor LeadingNote = 10

diatoneToPitch :: PitchClass -> Mode -> Octave -> Diatone -> Pitch
diatoneToPitch pc mode baseOct (deg, octave) = pitch $ (pcToInt pc) + (baseOct+octave)*12 + (diatoneToAbsPitch mode deg)

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
