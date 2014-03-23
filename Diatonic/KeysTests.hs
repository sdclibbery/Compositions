module Diatonic.KeysTests where
import Diatonic.Keys
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit

tests = TestLabel "Keys" $ TestList
	[ testPitchInKeyExamples
    , testPitchInKeyMajorChromatic
    , testPitchInKeyMajorDegrees
    , testPitchInKeyMinorChromatic
    , testPitchInKeyMinorDegrees
	]

testPitchInKeyExamples = TestLabel "pitchInKey" $ TestList
				[ test C		C Major 0
				, test D		C Major 1
				, test Fs		G Major 6
				, test Bf		F Major 3
				, test Ef		C Minor 2
				] where
					test e pc mo d = (show pc ++ show mo ++ show d) ~: e ~=? pitchInKey pc mo d

hasEveryDegree :: [PitchClass] -> Bool
hasEveryDegree pcs = all (flip elem degrees) allDegrees && length pcs == length allDegrees
    where
        degrees = map pcToBasePc pcs
        allDegrees = [C, D, E, F, G, A, B]

majorKeys :: [PitchClass]
majorKeys = [Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs]

testPitchInKeyMajorChromatic = TestLabel "pitchInKeyMajorChromatic" $ TestList tests
    where
        tests = [ test pc Major d | pc <- majorKeys, d <- [Tonic .. LeadingNote] ]
        test pc mo d = (show pc ++ show mo ++ show d) ~: expected ~=? (actual - root) `mod` 12
            where
                root = pcToInt pc
                actual = pcToInt $ pitchInKey pc mo (fromEnum d)
                expected = majorScale !! fromEnum d
                majorScale = [0, 2, 4, 5, 7, 9, 11]

testPitchInKeyMajorDegrees = TestLabel "pitchInKeyMajorDegrees" $ TestList tests
    where
        tests = [ test pc Major | pc <- majorKeys ]
        test pc mo = (show pc ++ show mo) ~: True ~=? hasEveryDegree scale
            where
                scale = map (pitchInKey pc mo) [0..6]

minorKeys :: [PitchClass]
minorKeys = [Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As]

testPitchInKeyMinorChromatic = TestLabel "pitchInKeyMinorChromatic" $ TestList tests
    where
        tests = [ test pc Minor d | pc <- minorKeys, d <- [Tonic .. LeadingNote] ]
        test pc mo d = (show pc ++ show mo ++ show d) ~: expected ~=? (actual - root) `mod` 12
            where
                root = pcToInt pc
                actual = pcToInt $ pitchInKey pc mo (fromEnum d)
                expected = minorScale !! fromEnum d
                minorScale = [0, 2, 3, 5, 7, 8, 10]

testPitchInKeyMinorDegrees = TestLabel "pitchInKeyMinorDegrees" $ TestList tests
    where
        tests = [ test pc Minor | pc <- minorKeys ]
        test pc mo = (show pc ++ show mo) ~: True ~=? hasEveryDegree scale
            where
                scale = map (pitchInKey pc mo) [0..6]

