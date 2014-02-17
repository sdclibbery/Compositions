module Diatonic.IntervalTests where
import Diatonic.Interval
import Diatonic.Diatone
import qualified Euterpea.Music.Note.Music as Music
import Test.HUnit

tests = TestLabel "Interval" $ TestList
	[ testIsValid
	, testIsConsonant
	, testInvert
	, testDiatonicIntervalMajor
	, testDiatonicIntervalMinor
	, testPitchIntervalMinor
	]

testIsValid = TestLabel "isValid" $ TestList
	[ test True		(MkInterval Perfect Unison)
	, test False	(MkInterval Major Unison)
	, test True		(MkInterval Major Third)
	, test False	(MkInterval Augmented Third)
	] where
		test e i = (show i) ~: e ~=? isValid i

testIsConsonant = TestLabel "isConsonant" $ TestList
	[ test False	(MkInterval Major Second)
	, test True		(MkInterval Major Third)
	, test True		(MkInterval Minor Third)
	, test True		(MkInterval Perfect Fourth)
	, test True		(MkInterval Perfect Fifth)
	, test False	(MkInterval Diminished Fifth)
	, test False	(MkInterval Augmented Fifth)
	, test True		(MkInterval Major Sixth)
	, test False	(MkInterval Major Seventh)
	] where
		test e i = (show i) ~: e ~=? isConsonant i

testInvert = TestLabel "invert" $ TestList
	[ test	(MkInterval Perfect Octave)		(MkInterval Perfect Unison)
	, test	(MkInterval Diminished Octave)	(MkInterval Augmented Unison)
	, test	(MkInterval Major Seventh)		(MkInterval Minor Second)
	, test	(MkInterval Minor Sixth)		(MkInterval Major Third)
	, test	(MkInterval Perfect Fifth)		(MkInterval Perfect Fourth)
	, test	(MkInterval Diminished Fourth)	(MkInterval Augmented Fifth)
	, test	(MkInterval Major Third)		(MkInterval Minor Sixth)
	, test	(MkInterval Major Second)		(MkInterval Minor Seventh)
	, test	(MkInterval Augmented Unison)	(MkInterval Diminished Octave)
	] where
		test e i = (show i) ~: e ~=? invert i

testDiatonicIntervalMajor = TestLabel "diatonicInterval Major" $ TestList
	[ test (MkInterval Perfect Unison)		(Tonic, 4) (Tonic, 4)
	, test (MkInterval Major Third)			(Tonic, 4) (Mediant, 4)
	, test (MkInterval Perfect Fifth)		(Tonic, 4) (Dominant, 4)
	, test (MkInterval Perfect Octave)		(Tonic, 4) (Tonic, 5)
	, test (MkInterval Perfect Octave)		(Tonic, 5) (Tonic, 4)
	, test (MkInterval Major Ninth)			(Tonic, 4) (SuperTonic, 5)
	, test (MkInterval Major Third)			(Tonic, 4) (Mediant, 5)
	, test (MkInterval Perfect Eleventh)	(Tonic, 4) (SubDominant, 5)
	, test (MkInterval Major Thirteenth)	(Tonic, 4) (SubMediant, 5)
	]
	where
		test e d1 d2 = show d1 ++ show d2 ~: e ~=? diatonicInterval Music.Major d1 d2

testDiatonicIntervalMinor = TestLabel "diatonicInterval Minor" $ TestList
	[ test (MkInterval Perfect Unison)		(Tonic, 4) (Tonic, 4)
	, test (MkInterval Minor Third)			(Tonic, 4) (Mediant, 4)
	, test (MkInterval Perfect Fifth)		(Tonic, 4) (Dominant, 4)
	, test (MkInterval Perfect Octave)		(Tonic, 4) (Tonic, 5)
	, test (MkInterval Perfect Octave)		(Tonic, 5) (Tonic, 4)
	, test (MkInterval Major Ninth)			(Tonic, 4) (SuperTonic, 5)
	, test (MkInterval Minor Third)			(Tonic, 4) (Mediant, 5)
	, test (MkInterval Perfect Eleventh)	(Tonic, 4) (SubDominant, 5)
	, test (MkInterval Minor Thirteenth)	(Tonic, 4) (SubMediant, 5)
	]
	where
		test e d1 d2 = show d1 ++ show d2 ~: e ~=? diatonicInterval Music.Minor d1 d2

testPitchIntervalMinor = TestLabel "pitchInterval Minor" $ TestList
	[ test (MkInterval Perfect Unison)		(Music.C, 4) (Music.C, 4)
	, test (MkInterval Minor Third)			(Music.C, 4) (Music.Ef, 4)
	, test (MkInterval Major Third)			(Music.C, 4) (Music.E, 4)
	, test (MkInterval Perfect Fifth)		(Music.C, 4) (Music.G, 4)
	, test (MkInterval Perfect Octave)		(Music.C, 4) (Music.C, 5)
	, test (MkInterval Perfect Octave)		(Music.C, 5) (Music.C, 4)
	, test (MkInterval Major Ninth)			(Music.C, 4) (Music.D, 5)
	, test (MkInterval Minor Third)			(Music.C, 4) (Music.Ef, 5)
	, test (MkInterval Perfect Eleventh)	(Music.C, 4) (Music.F, 5)
	, test (MkInterval Minor Thirteenth)	(Music.C, 4) (Music.Af, 5)
	]
	where
		test e p1 p2 = show p1 ++ show p2 ~: e ~=? pitchInterval p1 p2
