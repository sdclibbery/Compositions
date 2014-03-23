module Diatonic.DiatoneTests where
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Test.HUnit

tests = TestLabel "Diatone" $ TestList
	[ testDiatoneToPitch
	, testDiatoneOrd
	, testDiatoneToChromaticDelta
	, testTransposeDiatone
	]

testDiatoneToChromaticDelta = TestLabel "diatoneToChromaticDelta" $ TestList
				[ test 0		Major (dti 0)
				, test 12		Major (dti 1)
				, test 2		Major (dtii 0)
				, test 4		Major (dtiii 0)
				, test 3		Minor (dtiii 0)
				] where
					test e mo d = (show mo ++ show d) ~: e ~=? diatoneToChromaticDelta mo d

testDiatoneOrd = TestLabel "diatoneOrd" $ TestList
				[ test EQ			(dti 0) (dti 0)
				, test LT			(dti 0) (dtii 0)
				, test GT			(dtii 0) (dti 0)
				, test LT			(dti 0) (dti 1)
				, test GT			(dti 1) (dti 0)
				, test LT			(dti (-1)) (dti 0)
				] where
					test e l r = (show l ++ show r) ~: e ~=? l `compare` r

testDiatoneToPitch = TestLabel "diatoneToPitch" $ TestList
				[ test (C,4)	C Major (dti 4)
				, test (G,4)	G Major (dti 4)
				, test (Bf,4)	Bf Major (dti 4)
				, test (D,4)	C Major (dtii 4)
				, test (B,4)	C Major (dtvii 4)
				, test (Fs,5)	G Major (dtvii 4)
				, test (C,5)	Bf Major (dtii 4)
				, test (Ef,5)	Bf Major (dtiv 4)
				, test (C,5)	A Minor (dtiii 4)
				, test (C,5)	A Minor (dtiii 4)
				, test (Ef,4)	C Minor (dtiii 4)
				] where
					test e pc mo d = (show pc ++ show mo ++ show d) ~: e ~=? diatoneToPitch pc mo d

testTransposeDiatone = TestLabel "transposeDiatone" $ TestList
				[ test (dti 0)			0 (dti 0)
				, test (dtii 0)			1 (dti 0)
				, test (dtiii 0)		2 (dti 0)
				, test (dti 1)			7 (dti 0)
				, test (dtiv 0)			1 (dtiii 0)
				, test (dti 1)			1 (dtvii 0)
				, test (dtvii (-1))		(-1) (dti 0)
				] where
					test e o d = (show o ++ show d) ~: e ~=? transposeDiatone o d
