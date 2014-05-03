module Diatonic.Analysis.DeconstructTests where
import Diatonic.Analysis.Deconstruct
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit
import Data.Ratio

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
	[ test (Alto, Alto, [], Exact)											(es, es)
	, test (Alto, Alto, [], Mismatch)										(s 1, es)
	, test (Alto, Alto, [], Mismatch)										(es, s 1)
	, test (Alto, Alto, [], Mismatch)										(s 1, s 10)
	, test (Alto, Alto, [], Mismatch)										(s 10, s 1)
	, test (Alto, Alto, [ (1, "1", "1") ], Exact)							(s 1, s 1)
	, test (Alto, Alto, [ (1, "1", "1") ], Mismatch)						(s 1, s2 1)
	, test (Alto, Alto, [ (1, "1", "1") ], Mismatch)						(s2 1, s 1)
	, test (Alto, Alto, [ (6, "6", "n1") ], Mismatch)						(s2 1, s 6)
	, test (Alto, Alto, [ (2, "1", "2") ], Mismatch)						(s 1, s 2)
	, test (Alto, Alto, [ (2, "1", "2") ], Mismatch)						(s 2, s 1)
	, test (Alto, Alto, [ (2, "2", "2"), (3, "2", "n2") ], Mismatch)		(s 2, sh 2)
	, test (Alto, Alto, [ (2, "2", "2"), (3, "2", "n2") ], Mismatch)		(sh 2, s 2)
	, test (Alto, Alto, [ (2, "1", "2"), (3, "1", "n2") ], Mismatch)		(s 1, sh 2)
	, test (Alto, Alto, [ (2, "1", "2"), (3, "1", "n2") ], Mismatch)		(sh 2, s 1)
	, test (Alto, Alto, [ (1, "1", "1"), (2, "n1", "n1") ], Exact)			(sh 1, sh 1)
	, test (Alto, Alto, [ (2, "2", "n1") ], Mismatch)						(sh 1, sh 2)
	, test (Alto, Alto, [(1%1,"2","1"),(2%1,"2","n2"),(3%1,"n2","n1")], Mismatch)		(st 1 2, st 2 1) -- Start and end at same time, but note boundaries dont match up along the way
	] where
		test e ss = show ss ~: e ~=? correlateSequences ss
		es = Temporal Alto 1 ([] :: [(Event String)])
		s t = Temporal Alto t [ (5, show $ floor t) ]
		s2 t = Temporal Alto t [ (5, show $ floor t), (1, "n" ++ (show $ floor t)) ]
		sh t = Temporal Alto t [ (1, show $ floor t), (1, "n" ++ (show $ floor t)) ]
		st d1 d2 = Temporal Alto 1 [ (d1, show $ floor d1), (d2, "n" ++ (show $ floor d2)) ]

