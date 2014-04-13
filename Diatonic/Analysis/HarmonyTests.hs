module Diatonic.Analysis.HarmonyTests where
import Diatonic.Analysis.Harmony
import Diatonic.Analysis.Report
import Diatonic.Analysis.Deconstruct
import Diatonic.Diatone
import Diatonic.Interval as I
import Euterpea.Music.Note.Music as M
import Test.HUnit
import Data.Ratio

tests = TestLabel "Harmony" $ TestList
	[ testAnalyseHarmonyS96
	, testAllPairs
	, testToIntervalInfos
	]

testAnalyseHarmonyS96 = TestLabel "analyseHarmony S96" $ TestList
	[ test []													$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: iii 4 qn)
	, test [Error ps (1%4) (Harmony 96) "Consecutive unisons"]	$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 4 qn)
	, test []													$ (i 4 qn :+: i 4 qn) :=: (i 4 qn :+: i 4 qn)
	, test [Error ps (1%4) (Harmony 96) "Consecutive octaves"]	$ (i 3 qn :+: ii 3 qn) :=: (i 4 qn :+: ii 4 qn)
	, test []													$ (i 3 qn :+: i 3 qn) :=: (i 4 qn :+: i 4 qn)
	, test []													$ (rest en :+: i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 4 qn)
	, test [Error ps (1%1) (Harmony 96) "Consecutive unisons"]	$ (rest dhn :+: i 4 qn :+: ii 4 qn) :=: (i 4 wn :+: ii 4 wn)
	] where
		test e m = (show m) ~: e ~=? analyseMusic M.Major (musicToSequences m)
		ps = [Alto, Alto]

-- !!! Will need a test for the consecutive fifths case, whereby one part jumping an octave is NOT considered consecutive fifths... (see S96...)

testAllPairs = TestLabel "allPairs" $ TestList
	[ test []										$ ([] :: [()])
	, test []										$ [1]
	, test [(1,2)]									$ [1,2]
	, test [(1,2),(1,3),(2,3)]						$ [1,2,3]
	, test [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]	$ [1,2,3,4]
	] where
		test e xs = (show xs) ~: e ~=? allPairs xs

testToIntervalInfos = TestLabel "toIntervalInfos" $ TestList
	[ test []																		(Alto, Alto, [])
	, test []																		(Alto, Alto, [ (1, dti 4, dti 4) ])
	, test [(2, Similar, i8, i8, ((Alto, dti 3, dtii 3), (Alto, dti 4, dtii 4)))]	(Alto, Alto, [ (1, dti 3, dti 4), (2, dtii 3, dtii 4) ])
	, test [(2, Similar, i2, i3, ((Alto, dti 3, dtii 3), (Alto, dtii 3, dtiv 3)))]	(Alto, Alto, [ (1, dti 3, dtii 3), (2, dtii 3, dtiv 3) ])
	, test [(2, Oblique, iu, i3, ((Alto, dtii 3, dtii 3), (Alto, dtii 3, dtiv 3)))]	(Alto, Alto, [ (1, dtii 3, dtii 3), (2, dtii 3, dtiv 3) ])
	, test [(2, Contrary, iu, i8, ((Alto, dtii 3, dti 3), (Alto, dtii 3, dti 4)))]	(Alto, Alto, [ (1, dtii 3, dtii 3), (2, dti 3, dti 4) ])
	, test [(2, Contrary, iu, i8, ((Alto, dtii 3, dti 3), (Alto, dtii 3, dti 4))),
			(3, Oblique, i8, i2, ((Alto, dti 3, dti 3), (Alto, dti 4, dtii 3)))]	(Alto, Alto, [ (1, dtii 3, dtii 3), (2, dti 3, dti 4), (3, dti 3, dtii 3) ])
	] where
		test e xs = (show xs) ~: e ~=? toIntervalInfos M.Major xs
		iu = MkInterval Perfect Unison
		i2 = MkInterval I.Major Second
		i3 = MkInterval I.Minor Third
		i8 = MkInterval Perfect Octave
