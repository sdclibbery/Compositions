module Diatonic.DiatoneTests where
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Test.HUnit

tests = TestLabel "Diatone" $ TestList
	[ testDiatoneToPitch
	, testTransposeDiatone
	]

testDiatoneToPitch = TestLabel "diatoneToPitch" $ TestList
				[ test (C,4)	C Major 4 (Tonic, 0)
				, test (E,4)	C Major 4 (Mediant, 0)
				, test (C,5)	A Minor 4 (Mediant, 0)
--				, test (Ef,4)	C Minor 4 (Mediant, 0)
				] where
					test e pc mo o d = (show pc ++ show mo ++ show d) ~: e ~=? diatoneToPitch pc mo o d

testTransposeDiatone = TestLabel "transposeDiatone" $ TestList
				[ test (Tonic, 0)			0 (Tonic, 0)
				, test (SuperTonic, 0)		1 (Tonic, 0)
				, test (Mediant, 0)			2 (Tonic, 0)
				, test (Tonic, 1)			7 (Tonic, 0)
				, test (SubDominant, 0)		1 (Mediant, 0)
				, test (Tonic, 1)			1 (LeadingNote, 0)
				, test (LeadingNote, -1)	(-1) (Tonic, 0)
				] where
					test e o d = (show o ++ show d) ~: e ~=? transposeDiatone o d
