module Diatonic.Analysis.DeconstructTests where
import Diatonic.Analysis.Deconstruct
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit

tests = TestLabel "???" $ TestList
	[ testMusicToSequences
	, testCorrelateSequences
	]

testMusicToSequences = TestLabel "musicToSequences" $ TestList
				[ test []															$ (rest wn :: Music Diatone)
				, test [ Temporal 0 [ (1, dti 4) ] ]								$ i 4 wn
				, test [ Temporal 0 [ (1, dti 4), (1, dtii 4) ] ]					$ i 4 wn :+: ii 4 wn
				, test [ Temporal 0 [ (1, dti 4) ], Temporal 0 [ (1, dtii 4) ] ]	$ i 4 wn :=: ii 4 wn
				, test [ Temporal 0 [ (1, dti 4) ], Temporal 2 [ (1, dtii 4) ] ]	$ i 4 wn :+: rest wn :+: ii 4 wn
				] where
					test e m = (show m) ~: e ~=? musicToSequences m

testCorrelateSequences = TestLabel "correlateSequences" $ TestList
	[ test []										(es, es)
	, test []										(s 1, es)
	, test []										(es, s 1)
	, test []										(s 1, s 10)
	, test []										(s 10, s 1)
	, test [ (1, "1", "1") ]						(s 1, s 1)
	, test [ (2, "1", "2") ]						(s 1, s 2)
	, test [ (2, "1", "2") ]						(s 2, s 1)
	, test [ (2, "2", "2"), (3, "2", "n2") ]		(s 2, sh 2)
	, test [ (2, "2", "2"), (3, "2", "n2") ]		(sh 2, s 2)
	, test [ (2, "1", "2"), (3, "1", "n2") ]		(s 1, sh 2)
	, test [ (2, "1", "2"), (3, "1", "n2") ]		(sh 2, s 1)
	, test [ (1, "1", "1"), (2, "n1", "n1") ]		(sh 1, sh 1)
	, test [ (2, "2", "n1") ]						(sh 1, sh 2)
	] where
		test e ss = show ss ~: e ~=? correlateSequences ss
		es = Temporal 1 ([] :: [(Event String)])
		s t = Temporal t [ (5, show $ floor t) ]
		sh t = Temporal t [ (1, show $ floor t), (1, "n" ++ (show $ floor t)) ]

