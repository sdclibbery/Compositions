module Diatonic.IntervalTests where
import Diatonic.Interval
import Diatonic.Diatone
import qualified Euterpea.Music.Note.Music as Music

assert msg True = return ()
assert msg False = do print $ msg ++ " failed!"

testIsValid = do
	test $ MkInterval Perfect Unison
	testN $ MkInterval Major Unison -- No such thing :-)
	test $ MkInterval Major Third
	testN $ MkInterval Augmented Third -- No such thing :-)
	where
		test i = assert ("isValid Interval: "++(show i)) $ isValid $ i
		testN i = assert ("not isValid Interval: "++(show i)) $ not $ isValid $ i

testIsConsonant = do
	testN $ MkInterval Major Second
	test $ MkInterval Major Third
	test $ MkInterval Minor Third
	test $ MkInterval Perfect Fourth
	test $ MkInterval Perfect Fifth
	testN $ MkInterval Diminished Fifth
	testN $ MkInterval Augmented Fifth
	test $ MkInterval Major Sixth
	test $ MkInterval Major Sixth
	testN $ MkInterval Major Seventh
	where
		test i = assert ("isConsonant Interval: "++(show i)) $ isConsonant $ i
		testN i = assert ("not isConsonant Interval: "++(show i)) $ not $ isConsonant $ i

testInvert = do
	test (MkInterval Perfect Unison) (MkInterval Perfect Octave)
	test (MkInterval Augmented Unison) (MkInterval Diminished Octave)
	test (MkInterval Minor Second) (MkInterval Major Seventh)
	test (MkInterval Major Third) (MkInterval Minor Sixth)
	test (MkInterval Perfect Fourth) (MkInterval Perfect Fifth)
	test (MkInterval Augmented Fifth) (MkInterval Diminished Fourth)
	test (MkInterval Minor Sixth) (MkInterval Major Third)
	test (MkInterval Minor Seventh) (MkInterval Major Second)
	test (MkInterval Diminished Octave) (MkInterval Augmented Unison)
	where
		test i e = assert ("invert "++msg) $ e == actual
			where
				actual = invert i
				msg = ". expected: " ++ (show e) ++ ". actual: " ++ (show actual) ++ ". inputs: " ++ (show i)

testDiatonicIntervalMajor = do
	test (Tonic, 4) (Tonic, 4) (MkInterval Perfect Unison)
	test (Tonic, 4) (Mediant, 4) (MkInterval Major Third)
	test (Tonic, 4) (Dominant, 4) (MkInterval Perfect Fifth)
	test (Tonic, 4) (Tonic, 5) (MkInterval Perfect Octave)
	test (Tonic, 5) (Tonic, 4) (MkInterval Perfect Octave)
	test (Tonic, 4) (SuperTonic, 5) (MkInterval Major Ninth)
	test (Tonic, 4) (Mediant, 5) (MkInterval Major Third)
	test (Tonic, 4) (SubDominant, 5) (MkInterval Perfect Eleventh)
	test (Tonic, 4) (SubMediant, 5) (MkInterval Major Thirteenth)
	where
		test d1 d2 e = assert ("diatonicInterval major"++msg) $ e == actual
			where
				actual = diatonicInterval Music.Major d1 d2
				msg = ". expected: " ++ (show e) ++ ". actual: " ++ (show actual) ++ ". inputs: " ++ (show (d1, d2))

testDiatonicIntervalMinor = do
	test (Tonic, 4) (Tonic, 4) (MkInterval Perfect Unison)
	test (Tonic, 4) (Mediant, 4) (MkInterval Minor Third)
	test (Tonic, 4) (Dominant, 4) (MkInterval Perfect Fifth)
	test (Tonic, 4) (Tonic, 5) (MkInterval Perfect Octave)
	test (Tonic, 5) (Tonic, 4) (MkInterval Perfect Octave)
	test (Tonic, 4) (SuperTonic, 5) (MkInterval Major Ninth)
	test (Tonic, 4) (Mediant, 5) (MkInterval Minor Third)
	test (Tonic, 4) (SubDominant, 5) (MkInterval Perfect Eleventh)
	test (Tonic, 4) (SubMediant, 5) (MkInterval Minor Thirteenth)
	where
		test d1 d2 e = assert ("diatonicInterval minor"++msg) $ e == actual
			where
				actual = diatonicInterval Music.Minor d1 d2
				msg = ". expected: " ++ (show e) ++ ". actual: " ++ (show actual) ++ ". inputs: " ++ (show (d1, d2))

run = do
	print "Interval Tests: Start"
	testIsValid
	testIsConsonant
	testInvert
	testDiatonicIntervalMajor
	testDiatonicIntervalMinor
	print "Interval Tests: Done!"

