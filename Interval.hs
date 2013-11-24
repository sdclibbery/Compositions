-- Interval based music: easy to transpose within a key

module Interval where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic

type Degree = Int
type Interval = (Degree, Octave)

interval :: Degree -> Octave -> Interval
interval deg octave = (degM, octave + octaveO)
	where
		degZ = deg - 1
		degM = (degZ `mod` 7) + 1
		octaveO = degZ `div` 7

i,ii,iii,iv,v,vi,vii :: Octave -> Dur -> Music Interval
i octave dur = note dur $ interval 1 octave
ii octave dur = note dur $ interval 2 octave
iii octave dur = note dur $ interval 3 octave
iv octave dur = note dur $ interval 4 octave
v octave dur = note dur $ interval 5 octave
vi octave dur = note dur $ interval 6 octave
vii octave dur = note dur $ interval 7 octave

intToAbsPitch :: Mode -> Degree -> AbsPitch
intToAbsPitch Major 1 = 0
intToAbsPitch Major 2 = 2
intToAbsPitch Major 3 = 4
intToAbsPitch Major 4 = 5
intToAbsPitch Major 5 = 7
intToAbsPitch Major 6 = 9
intToAbsPitch Major 7 = 11
intToAbsPitch Minor 1 = 0
intToAbsPitch Minor 2 = 2
intToAbsPitch Minor 3 = 3
intToAbsPitch Minor 4 = 5
intToAbsPitch Minor 5 = 7
intToAbsPitch Minor 6 = 8
intToAbsPitch Minor 7 = 10

intToPitch :: PitchClass -> Mode -> Octave -> Interval -> Pitch
intToPitch pc mode baseOct (deg, octave) = pitch $ (pcToInt pc) + (baseOct+octave)*12 + (intToAbsPitch mode deg)

mIntToPitch :: PitchClass -> Mode -> Octave -> Music Interval -> Music Pitch
mIntToPitch pc mode octave m = mMap (intToPitch pc mode octave) m

intTranspose :: Int -> Interval -> Interval
intTranspose offset (deg, octave) = interval (deg+offset) octave

mTranspose :: Music Interval -> Int -> Music Interval
mTranspose m offset = mMap (intTranspose offset) m 

