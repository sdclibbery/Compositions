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
music :: [[Event]] -> Music
music ess
  | length ess == 1 = Music $ map makePart $ zip [Bass] ess
  | length ess == 2 = Music $ map makePart $ zip [Bass, Treble] ess
  | length ess == 3 = Music $ map makePart $ zip [Bass, Alto, Treble] ess
  | length ess == 4 = Music $ map makePart $ zip [Bass, Tenor, Alto, Treble] ess
  where
    makePart (n, es) = Part n es


-- |Lengthen a note or rest by a given multiplier. Eg .>2 doubles the events duration.
(.>) :: Event -> Integer -> Event
(Play d n) .> i = Play (d * (i%1)) n
(Rest d) .> i = Rest (d * (i%1))

-- |Lengthen a list of notes or rests by a given multiplier. Eg .>>2 doubles all the events duration.
(.>>) :: [Event] -> Integer -> [Event]
es .>> i = map (.> i) es


-- |Shorten a note or rest by a given multiplier. Eg .<2 halves the events duration.
(.<) :: Event -> Integer -> Event
(Play d n) .< i = Play (d * (1%i)) n
(Rest d) .< i = Rest (d * (1%i))

-- |Shorten a list of notes or rests by a given multiplier. Eg .<<2 halves all the events duration.
(.<<) :: [Event] -> Integer -> [Event]
es .<< i = map (.< i) es


-- |Shorthand for rests
rw = Rest (1)
rh = Rest (1%2)
r = Rest (1%4)
re = Rest (1%8)

-- |Take a note and turn it into an event
wn = Play (1)
hn = Play (1%2)
qn = Play (1%4)
en = Play (1%8)

