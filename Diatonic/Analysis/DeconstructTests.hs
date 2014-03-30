module Diatonic.Analysis.DeconstructTests where
import Diatonic.Analysis.Deconstruct
import Diatonic.Diatone
import Euterpea.Music.Note.Music
import Test.HUnit

tests = TestLabel "???" $ TestList
	[ testMusicToSequences
	]

testMusicToSequences = TestLabel "musicToSequences" $ TestList
				[ test []															$ (rest wn :: Music Diatone)
				, test [ Temporal 0 [ (1, dti 4) ] ]								$ i 4 wn
				, test [ Temporal 0 [ (1, dti 4), (1, dtii 4) ] ]					$ i 4 wn :+: ii 4 wn
				, test [ Temporal 0 [ (1, dti 4) ], Temporal 0 [ (1, dtii 4) ] ]	$ i 4 wn :=: ii 4 wn
				, test [ Temporal 0 [ (1, dti 4) ], Temporal 2 [ (1, dtii 4) ] ]	$ i 4 wn :+: rest wn :+: ii 4 wn
				] where
					test e m = (show m) ~: e ~=? musicToSequences m
