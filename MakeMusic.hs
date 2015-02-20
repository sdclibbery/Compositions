{-|
Module      : MakeMusic
Description : Definition of musical structure for use in harmonic analysis
-}

module MakeMusic (
  (.>),
  (.>>),
  (.<),
  (.<<),
  music,
  rw, r, rh, re,
  wn, hn, qn, en
) where
import Music
import Note
import Data.List
import Data.Ord
import Data.Ratio

-- |Make a music from lists of events. Each list of events is assigned a default part name
music :: [[SeqEvent]] -> Music
music ess = foldl' (\m (pn, es) -> addPart m pn es) emptyMusic $ annotate ess
  where
    addPart m pn es = foldl' (\m e -> addEvent m pn e) m es
    annotate ess
      | length ess == 1 = [(Bass, ess!!0)]
      | length ess == 2 = [(Bass, ess!!0), (Soprano, ess!!1)]
      | length ess == 3 = [(Bass, ess!!0), (Alto, ess!!1), (Soprano, ess!!2)]
      | length ess == 4 = [(Bass, ess!!0), (Tenor, ess!!1), (Alto, ess!!2), (Soprano, ess!!3)]

-- |Lengthen a note or rest by a given multiplier. Eg .>2 doubles the SeqEvents duration.
(.>) :: SeqEvent -> Integer -> SeqEvent
(SeqPlay d n) .> i = SeqPlay (d * (i%1)) n
(SeqRest d) .> i = SeqRest (d * (i%1))

-- |Lengthen a list of notes or rests by a given multiplier. Eg .>>2 doubles all the SeqEvents duration.
(.>>) :: [SeqEvent] -> Integer -> [SeqEvent]
es .>> i = map (.> i) es


-- |Shorten a note or rest by a given multiplier. Eg .<2 halves the SeqEvents duration.
(.<) :: SeqEvent -> Integer -> SeqEvent
(SeqPlay d n) .< i = SeqPlay (d * (1%i)) n
(SeqRest d) .< i = SeqRest (d * (1%i))

-- |Shorten a list of notes or rests by a given multiplier. Eg .<<2 halves all the SeqEvents duration.
(.<<) :: [SeqEvent] -> Integer -> [SeqEvent]
es .<< i = map (.< i) es


-- |Shorthand for rests
rw = SeqRest (1)
rh = SeqRest (1%2)
r = SeqRest (1%4)
re = SeqRest (1%8)

-- |Take a note and turn it into an SeqEvent
wn = SeqPlay (1)
hn = SeqPlay (1%2)
qn = SeqPlay (1%4)
en = SeqPlay (1%8)

