module Analysis.MelodyTests where
import Test.HUnit

tests = TestLabel "Melody" $ TestList
	[ testAnalyseMelodyS89
	]

testAnalyseMelodyS89 = TestLabel "analyseMelody S89" $ TestList
	[ --test []															$ i 4 qn :+: ii 4 qn
	] --where
		--test e m = (show m) ~: e ~=? analyseMusic Major (musicToSequences m)

