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
	[ testAllPairs
	, testToIntervalInfos
	, testAnalyseHarmonyS96
	, testAnalyseHarmonyS98
	]

testAnalyseHarmonyS96 = TestLabel "analyseHarmony S96" $ TestList
	[ test []													$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: iii 4 qn :+: i 1 qn)
	, test [Error ps (1%4) (Harmony 96) "Consecutive unisons"]	$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 4 qn :+: i 1 qn)
	, test []													$ (i 4 qn :+: i 4 qn) :=: (i 4 qn :+: i 4 qn :+: i 1 qn)
	, test [Error ps (1%4) (Harmony 96) "Consecutive octaves"]	$ (i 3 qn :+: ii 3 qn) :=: (i 4 qn :+: ii 4 qn :+: i 1 qn)
-- !!! This next one is surprising and needs fixing :-)
-- It should be caught by the 'Repeat' pattern but its not :-/
-- LOOK AT THE DEBUG output: the comparison in toIntervalInfos seems wrong
	, test []													$ (i 3 qn :+: i 3 qn) :=: (i 4 qn :+: i 4 qn :+: i 1 qn)
	, test []													$ (rest en :+: i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 4 qn :+: i 1 qn)
	, test [Error ps (1%1) (Harmony 96) "Consecutive unisons"]	$ (rest dhn :+: i 4 qn :+: ii 4 qn) :=: (i 4 wn :+: ii 4 wn :+: i 1 qn)
	, test []													$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 5 qn :+: i 1 qn)
	, test []													$ (i 4 qn :+: ii 4 qn) :=: (i 5 qn :+: ii 6 qn :+: i 1 qn)
	] where
		test e m = (show m) ~: e ~=? analyseMusic M.Major (musicToSequences m)
		ps = [Alto, Alto]

-- Consecutive unisons/octaves dont count if the entire parts double each other
testAnalyseHarmonyS98 = TestLabel "analyseHarmony S98" $ TestList
	[ test []													$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 4 qn)
	, test [Error ps (1%4) (Harmony 96) "Consecutive unisons"]	$ (i 4 qn :+: ii 4 qn) :=: (i 4 qn :+: ii 4 qn :+: i 1 qn)
-- !!! This next one needs proper work; just bailing on an 'Exact' isnt good enough; must check for identical intervals at every step
	, test [Error ps (1%4) (Harmony 96) "Consecutive unisons"]	$ (i 4 qn :+: ii 4 qn :+: i 4 qn) :=: (i 4 qn :+: ii 4 qn :+: iii 4 qn)
	, test []													$ (i 4 qn :+: ii 4 qn) :=: (i 5 qn :+: ii 5 qn)
	, test [Error ps (1%4) (Harmony 96) "Consecutive octaves"]	$ (i 4 qn :+: ii 4 qn) :=: (i 5 qn :+: ii 5 qn :+: i 1 qn)
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
	[ test []																				(Alto, Alto, [], Exact)
	, test []																				(Alto, Alto, [ (1, dti 4, dti 4) ], Exact)
	, test [(2, Similar, i8, i8, ((Alto, dti 3, dtii 3), (Alto, dti 4, dtii 4), Exact))]	(Alto, Alto, [ (1, dti 3, dti 4), (2, dtii 3, dtii 4) ], Exact)
	, test [(2, Repeat, i8, i8, ((Alto, dti 3, dti 3), (Alto, dti 4, dti 4), Exact))]		(Alto, Alto, [ (1, dti 3, dti 4), (2, dti 3, dti 4) ], Exact)
	, test [(2, Similar, i2, i3, ((Alto, dti 3, dtii 3), (Alto, dtii 3, dtiv 3), Exact))]	(Alto, Alto, [ (1, dti 3, dtii 3), (2, dtii 3, dtiv 3) ], Exact)
	, test [(2, Oblique, iu, i3, ((Alto, dtii 3, dtii 3), (Alto, dtii 3, dtiv 3), Exact))]	(Alto, Alto, [ (1, dtii 3, dtii 3), (2, dtii 3, dtiv 3) ], Exact)
	, test [(2, Contrary, iu, i8, ((Alto, dtii 3, dti 3), (Alto, dtii 3, dti 4), Exact))]	(Alto, Alto, [ (1, dtii 3, dtii 3), (2, dti 3, dti 4) ], Exact)
	, test [(2, Contrary, iu, i8, ((Alto, dtii 3, dti 3), (Alto, dtii 3, dti 4), Exact)),
			(3, Oblique, i8, i2, ((Alto, dti 3, dti 3), (Alto, dti 4, dtii 3), Exact))]		(Alto, Alto, [ (1, dtii 3, dtii 3), (2, dti 3, dti 4), (3, dti 3, dtii 3) ], Exact)
	] where
		test e xs = (show xs) ~: e ~=? toIntervalInfos M.Major xs
		iu = MkInterval Perfect Unison
		i2 = MkInterval I.Major Second
		i3 = MkInterval I.Minor Third
		i8 = MkInterval Perfect Octave
