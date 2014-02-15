module Diatonic.DiatoneTests where
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic

assert msg True = return ()
assert msg False = do print $ msg ++ " failed!"

testDiatoneToPitch = do
	test C Major 4 (Tonic, 0) (C, 4)
	test C Major 4 (Mediant, 0) (E, 4)
	test A Minor 4 (Mediant, 0) (C, 5)
--	test C Minor 4 (Mediant, 0) (Ef, 4)
	where
		test pc m o d e = assert ("diatoneToPitch"++msg) $ e == actual
			where
				actual = diatoneToPitch pc m o d
				msg = ". expected: " ++ (show e) ++ ". actual: " ++ (show actual) ++ ". inputs: " ++ (show (pc, m, o, d))

testTransposeDiatone = do
	test 0 (Tonic, 0) (Tonic, 0)
	test 1 (Tonic, 0) (SuperTonic, 0)
	test 2 (Tonic, 0) (Mediant, 0)
	test 7 (Tonic, 0) (Tonic, 1)
	test 1 (Mediant, 0) (SubDominant, 0)
	test 1 (LeadingNote, 0) (Tonic, 1)
	test (-1) (Tonic, 0) (LeadingNote, -1)
	where
		test o d e = assert ("transposeDiatone"++msg) $ e == actual
			where
				actual = transposeDiatone o d
				msg = ". expected: " ++ (show e) ++ ". actual: " ++ (show actual) ++ ". inputs: " ++ (show (o, d))

run = do
	print "Diatone Tests Start"
	testDiatoneToPitch
	testTransposeDiatone
	print "Diatone Tests Done!"
