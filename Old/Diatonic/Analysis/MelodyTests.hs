module Diatonic.Analysis.MelodyTests where
import Diatonic.Analysis.Melody
import Diatonic.Analysis.Report
import Diatonic.Analysis.Deconstruct
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit
import Data.Ratio

tests = TestLabel "Melody" $ TestList
	[ testAnalyseMusic
	, testAnalyseMelodyS89
	, testAnalyseMelodyS90
	, testAnalyseMelodyS91
	, testAnalyseMelodyS92
	]

testAnalyseMusic = TestLabel "analyseMusic" $ TestList
	[ test []															$ rest qn
	, test []															$ rest qn :+: ii 4 qn
	, test [Error [Alto] (1%4) (Harmony 89) "Dissonance MajorSeventh"]	$ i 4 qn :+: vii 5 qn
	, test [Error [Alto] (5%4) (Harmony 89) "Dissonance MajorSeventh"]	$ rest wn :+: i 4 qn :+: vii 5 qn
	, test []															$ i 4 wn :+: i 4 qn :+: rest wn
	, test [Error [Alto] (13%4) (Harmony 89) "Dissonance MajorSeventh"]	$ rest wn :+: i 4 wn :+: rest wn :+: i 4 qn :+: vii 5 qn :+: rest wn
	, test [Error [Alto] (1%4) (Harmony 89) "Dissonance MajorSeventh", Error [Alto] (7%4) (Harmony 89) "Dissonance MajorSeventh"]	$ i 4 qn :+: vii 5 qn :+: rest wn :+: i 4 qn :+: vii 5 qn
	, test [Error [Alto] (1%4) (Harmony 89) "Dissonance MajorSeventh", Error [Alto] (1%4) (Harmony 89) "Dissonance MajorSeventh"]	$ (i 4 qn :+: vii 5 qn) :=: (i 4 qn :+: vii 5 qn)
	] where
		test e m = (show m) ~: e ~=? analyseMusic Major (musicToSequences m)

testAnalyseMelodyS89 = TestLabel "analyseMusic S89" $ TestList
	[ test []															$ i 4 qn :+: ii 4 qn
	, test []															$ ii 4 qn :+: i 4 qn
	, test [Error [Alto] (1%4) (Harmony 89) "Dissonance MajorSeventh"]	$ i 4 qn :+: vii 5 qn
	, test [Error [Alto] (5%4) (Harmony 89) "Dissonance MajorSeventh"]	$ i 4 wn :+: i 4 qn :+: vii 5 qn
	] where
		test e m = (show m) ~: e ~=? analyseMusic Major (musicToSequences m)

testAnalyseMelodyS90 = TestLabel "analyseMusic S90" $ TestList
	[ test [Warning [Alto] (1%4) (Harmony 90) "Dissonance DiminishedFifth"]					$ vii 4 qn :+: iv 5 qn
	, test []																			$ vii 4 qn :+: iv 5 qn :+: iii 5 qn
	, test [Error [Alto] (1%4) (Harmony 90) "Unresolved Dissonance DiminishedFifth"]	$ vii 4 qn :+: iv 5 qn :+: i 5 qn
	, test []																			$ iv 5 qn :+: vii 4 qn :+: i 5 qn
	, test [Error [Alto] (1%4) (Harmony 90) "Unresolved Dissonance DiminishedFifth"]	$ iv 5 qn :+: vii 4 qn :+: iii 5 qn
	, test [Error [Alto] (1%4) (Harmony 90) "Outside Dissonance DiminishedFifth"]		$ vii 4 qn :+: iv 5 qn :+: vi 4 qn
	, test [Error [Alto] (1%4) (Harmony 90) "Outside Dissonance DiminishedFifth"]		$ vii 4 qn :+: iv 5 qn :+: v 5 qn
	] where
		test e m = (show m) ~: e ~=? analyseMusic Major (musicToSequences m)

testAnalyseMelodyS91 = TestLabel "analyseMusic S91" $ TestList
	[ test [Error [Alto] (1%4) (Harmony 91) "Dissonance AugmentedFourth"]	Major $ iv 4 qn :+: vii 4 qn
	, test []																Minor $ v 4 qn :+: vi 4 qn -- Augmented second is OK 
	] where
		test e mo m = (show m) ~: e ~=? analyseMusic mo (musicToSequences m)

testAnalyseMelodyS92 = TestLabel "analyseMusic S92" $ TestList
	[ test []															Major $ v 4 qn :+: iii 4 qn :+: iii 5 qn :+: i 5 qn
	, test [Error [Alto] (2%4) (Harmony 92) "Large Interval Approach"]	Major $ ii 4 qn :+: iii 4 qn :+: iii 5 qn
	, test [Error [Alto] (1%4) (Harmony 92) "Large Interval Leave"]		Major $ iii 4 qn :+: iii 5 qn :+: iv 5 qn
	] where
		test e mo m = (show m) ~: e ~=? analyseMusic mo (musicToSequences m)
