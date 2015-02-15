import Music
import Note
import Interval
import Intervals
import Notes
import Midi
import Key
import Keys
import Chord
import Numeral
import Numerals
import VoiceLeading
import Data.Ratio
import Data.List
import Data.Ord


type Progression = [(Key, [Numeral])]

progressionToChords :: Progression -> [Chord]
progressionToChords = concatMap numeralsToChords
  where
    numeralsToChords (k, ns) = map (numeralToChord k) ns


type Parts = ([Event], [Event], [Event])

concatParts :: Parts -> Parts -> Parts
concatParts (b, t, tr) (b', t', tr') = (b++b', t++t', tr++tr')


twice :: [a] -> [a]
twice xs = xs ++ xs



type Vertical = [Note]

voiceLead :: [Vertical] -> [Vertical]
voiceLead vs = vs


-- ** Refactors
-- Rename Structure module to Music
--
-- Rename Event to SeqEvent
--
-- New Duration type distinct from Time: use for SeqEvent durations
--
-- Rename Treble to Soprano
--
-- New Event type
-- Stored in Music instead of SeqEvent
-- addEvent function to add an event to a part
--  Used by music function
-- Event contains full context: PartName, start/end times, higher/lower parts at start time, earlier/later events in same part
-- 
-- getPartsAt function that gets Events in all parts at a given time
-- Midi output navigates Events rather than pattern matching into Music itself
-- 
-- Analysis uses new Event etc
-- 
-- Hide Music internals
-- 
-- MakeMusic module does existing compose from sequence
-- 




-- Consider Structure representation; want to be able to ask questions like:
-- Get notes annotated with time, part etc
-- Get next/prev note in any part
-- Get next/prev note in any part
-- Get notes in all parts at current time
-- Get next/prev notes in this and another part


-- !New voice leading module...
-- Sort out the analysis. Suggest we define new sets of clearer rules :-)


-- !!! Suggested approach to analysis:
--  Do whats good for composition: Give a music, and a potential note to add, and ask what errors/warnings would result
--  Can then also use this for analysis by stepping through a whole piece building it up (IS THIS TRUE??)

{-
parts should be in their ranges
parts mustnt cross
parts mustnt overlap
avoid large leaps
avoid unisons between parts
never have consecutive octaves or fifths (including compounds)
never more than four consecutive of any interval
avoid going to perfect consonance by similar motion (hidden octaves)
-}

---
-- NotesToParts should reassign notes to parts, transposing up or down by octaves as needed, to achieve good voice leading and part writing
--   Start by transposing the bass to be as close to the previous note as possible
-- Experiment with arpeggiation
-- Consider adding a level of abstraction on top of Numerals: chord functions. Specify a progression functionally and render down to Numerals

-- C Major prelude - Book one, well tempered clavier
progression :: Progression
progression = [
    (keyOfC, [ _I, _ii7^d, _V7^b, _I, _vi^b, _V7^d/V ]),
    (keyOfG, [ _I^b, _ii7, _V7, _I, _vio7, _ii^c/IV, _viio7^c/IV ]),
    (keyOfC, [ _I^b, _IVmaj7^d, _ii7, _V7, _I, _I7, _IVmaj7, _vio7^d, _viio7^d, _V7, _I^c, _V7sus4, _V7, _vio7add7^e, _I^c, _V7sus4, _V7, _I7 ])
    ]
  where
    _vio7add7 = numeral VI [_m3, _d5, _M6, _m7] First
    (^) = (.^)
    (/) = (./)
    b = _b
    c = _c
    d = _d
    e = _e

coda = (
    [c__, c__, c__].>>4,
    [r.<4, c_.>15.<4, r.<4, c_.>15.<4, c_.>4],
    [r.>2, f_, a_, c, f, c_, a_, c_, a_, f_, a_, f_, d_, f_, d_, r.>2, g, b, d', f', d', b, d', b, g, b, d, f, e, d].<<4 ++ [c'.>4]
  )

notesToParts :: [Event] -> Parts
notesToParts (ba:ns) = ([ba], [head ns], tail ns)

arpeggiateParts :: Parts -> Parts
arpeggiateParts ([ba],[te],trs) = (bass, tenor, treble)
  where
    bass = twice [ba.>2]
    tenor = twice [r.<4, te.>7.<4]
    treble = twice (r.<2 : twice (trs.<<4))

extendTo5Notes :: [Note] -> [Note]
extendTo5Notes (a:b:c:[]) = a:b:c:up a:up b:[]
extendTo5Notes (a:b:c:d:[]) = a:b:c:d:up a:[]
extendTo5Notes (a:b:c:d:e:[]) = a:b:c:d:e:[]
up = modifyOctave 1

prelude = music $ map (.>>2) [ bass, tenor, treble ]
  where
    (bass, tenor, treble) = concatParts body coda
    concatParts (bs',ts',trs') (bs,ts,trs) = (bs'++bs, ts'++ts, trs'++trs)
    progressionNotes = map (extendTo5Notes . chordToNotes) $ progressionToChords progression
    body = foldr (concatParts . arpeggiateParts . notesToParts . map qn) ([],[],[]) $ voiceLead progressionNotes



outputMidi = createMidi "test.midi" $ prelude

{-
performAnalysis = putStrLn $ pretty reports
  where
    reports = sortBy (comparing startTime) $ Melody.analyse prelude ++ Harmony.analyse prelude
    pretty rs = intercalate "\n" $ map show rs
-}

main = do
--  performAnalysis
  outputMidi
