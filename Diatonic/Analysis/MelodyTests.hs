module Diatonic.Analysis.MelodyTests where
import Diatonic.Analysis.Melody
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit
import Data.Ratio

tests = TestLabel "Melody" $ TestList
	[ testAnalyseMelodyS89
	, testAnalyseMelodyS90
	, testAnalyseMelodyS91
	]

testAnalyseMelodyS89 = TestLabel "analyseMelody S89" $ TestList
				[ test []									$ i 4 qn :+: ii 4 qn
				, test []									$ ii 4 qn :+: i 4 qn
				, test []									$ rest qn
				, test []									$ rest qn :+: ii 4 qn
				, test [Error 0 "Dissonance MajorSeventh"]	$ i 4 qn :+: vii 5 qn
				, test [Error 1 "Dissonance MajorSeventh"]	$ rest wn :+: i 4 qn :+: vii 5 qn
				, test [Error 1 "Dissonance MajorSeventh"]	$ i 4 wn :+: i 4 qn :+: vii 5 qn
				, test [Error 2 "Dissonance MajorSeventh"]	$ i 4 wn :+: rest wn :+: i 4 qn :+: vii 5 qn
--				, test [Error 0 "Dissonance MajorSeventh", Error 2 "Dissonance MajorSeventh"]	$ i 4 qn :+: vii 5 qn :+: rest wn :+: i 4 qn :+: vii 5 qn
				] where
					test e m = (show m) ~: e ~=? analyseMelody Major m

testAnalyseMelodyS90 = TestLabel "analyseMelody S90" $ TestList
				[ test [Warning 0 "Dissonance DiminishedFifth"]				$ vii 4 qn :+: iv 5 qn
				, test []													$ vii 4 qn :+: iv 5 qn :+: iii 5 qn
				, test [Error 0 "Unresolved Dissonance DiminishedFifth"]	$ vii 4 qn :+: iv 5 qn :+: i 5 qn
				, test []													$ iv 5 qn :+: vii 4 qn :+: i 5 qn
				, test [Error 0 "Unresolved Dissonance DiminishedFifth"]	$ iv 5 qn :+: vii 4 qn :+: iii 5 qn
				, test [Error 0 "Outside Dissonance DiminishedFifth"]		$ vii 4 qn :+: iv 5 qn :+: vi 4 qn
				, test [Error 0 "Outside Dissonance DiminishedFifth"]		$ vii 4 qn :+: iv 5 qn :+: v 5 qn
				] where
					test e m = (show m) ~: e ~=? analyseMelody Major m

testAnalyseMelodyS91 = TestLabel "analyseMelody S91" $ TestList
				[ test [Error 0 "Dissonance AugmentedFourth"]	Major $ iv 4 qn :+: vii 4 qn
				, test []										Minor $ v 4 qn :+: vi 4 qn -- Augmented second is OK 
				] where
					test e mo m = (show m) ~: e ~=? analyseMelody mo m
