module Diatonic.Analysis.DeconstructTests where
import Diatonic.Analysis.Deconstruct
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit

tests = TestLabel "DeconstructTests" $ TestList
	[ testMusicToSequences
	, testPartsToSequences
	, testCorrelateSequences
	]

testPartsToSequences = TestLabel "partsToSequences" $ TestList
				[ test [ t Treble 0 [ (1, dti 4) ] ]								$ [ (Treble, i 4 wn) ]
				, test [ t Alto 0 [ (1, dti 3) ], t Treble 0 [ (1, dti 4) ] ]		$ [ (Alto, i 3 wn), (Treble, i 4 wn) ]
				] where
					test e m = (show m) ~: e ~=? partsToSequences m
					t = Temporal

testMusicToSequences = TestLabel "musicToSequences" $ TestList
				[ test []														$ (rest wn :: Music Diatone)
				, test [ t Alto 0 [ (1, dti 4) ] ]								$ i 4 wn
				, test [ t Alto 0 [ (1, dti 4), (1, dtii 4) ] ]					$ i 4 wn :+: ii 4 wn
				, test [ t Alto 0 [ (1, dti 4) ], t Alto 0 [ (1, dtii 4) ] ]	$ i 4 wn :=: ii 4 wn
				, test [ t Alto 0 [ (1, dti 4) ], t Alto 2 [ (1, dtii 4) ] ]	$ i 4 wn :+: rest wn :+: ii 4 wn
				] where
					test e m = (show m) ~: e ~=? musicToSequences m
					t = Temporal

testCorrelateSequences = TestLabel "correlateSequences" $ TestList
	[ test (Alto, Alto, [])										(es, es)
	, test (Alto, Alto, [])										(s 1, es)
	, test (Alto, Alto, [])										(es, s 1)
	, test (Alto, Alto, [])										(s 1, s 10)
	, test (Alto, Alto, [])										(s 10, s 1)
	, test (Alto, Alto, [ (1, "1", "1") ])						(s 1, s 1)
	, test (Alto, Alto, [ (2, "1", "2") ])						(s 1, s 2)
	, test (Alto, Alto, [ (2, "1", "2") ])						(s 2, s 1)
	, test (Alto, Alto, [ (2, "2", "2"), (3, "2", "n2") ])		(s 2, sh 2)
	, test (Alto, Alto, [ (2, "2", "2"), (3, "2", "n2") ])		(sh 2, s 2)
	, test (Alto, Alto, [ (2, "1", "2"), (3, "1", "n2") ])		(s 1, sh 2)
	, test (Alto, Alto, [ (2, "1", "2"), (3, "1", "n2") ])		(sh 2, s 1)
	, test (Alto, Alto, [ (1, "1", "1"), (2, "n1", "n1") ])		(sh 1, sh 1)
	, test (Alto, Alto, [ (2, "2", "n1") ])						(sh 1, sh 2)
	] where
		test e ss = show ss ~: e ~=? correlateSequences ss
		es = Temporal Alto 1 ([] :: [(Event String)])
		s t = Temporal Alto t [ (5, show $ floor t) ]
		sh t = Temporal Alto t [ (1, show $ floor t), (1, "n" ++ (show $ floor t)) ]

