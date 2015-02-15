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
music ess
  | length ess == 1 = Music (makePart Bass 0) (empty Tenor) (empty Alto) (empty Soprano)
  | length ess == 2 = Music (makePart Bass 0) (empty Tenor) (empty Alto) (makePart Bass 1)
  | length ess == 3 = Music (makePart Bass 0) (empty Tenor) (makePart Alto 1) (makePart Bass 2)
  | length ess == 4 = Music (makePart Bass 0) (makePart Tenor 1) (makePart Alto 2) (makePart Bass 3)
  where
    makePart p i = Part p (ess!!i)
    empty p = Part p []


-- |Lengthen a note or rest by a given multiplier. Eg .>2 doubles the SeqEvents duration.
(.>) :: SeqEvent -> Integer -> SeqEvent
(Play d n) .> i = Play (d * (i%1)) n
(Rest d) .> i = Rest (d * (i%1))

-- |Lengthen a list of notes or rests by a given multiplier. Eg .>>2 doubles all the SeqEvents duration.
(.>>) :: [SeqEvent] -> Integer -> [SeqEvent]
es .>> i = map (.> i) es


-- |Shorten a note or rest by a given multiplier. Eg .<2 halves the SeqEvents duration.
(.<) :: SeqEvent -> Integer -> SeqEvent
(Play d n) .< i = Play (d * (1%i)) n
(Rest d) .< i = Rest (d * (1%i))

-- |Shorten a list of notes or rests by a given multiplier. Eg .<<2 halves all the SeqEvents duration.
(.<<) :: [SeqEvent] -> Integer -> [SeqEvent]
es .<< i = map (.< i) es


-- |Shorthand for rests
rw = Rest (1)
rh = Rest (1%2)
r = Rest (1%4)
re = Rest (1%8)

-- |Take a note and turn it into an SeqEvent
wn = Play (1)
hn = Play (1%2)
qn = Play (1%4)
en = Play (1%8)

