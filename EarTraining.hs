import Music
import MakeMusic
import Note
import Interval
import Intervals
import Notes
import Midi
import Key
import Keys
import Data.Ratio
import Data.List
import Data.Ord
import System.Random

-- Output relevant files to support ear training as suggested in http://www.miles.be/articles/7-ear-training-a-direct-and-logical-path


scale = [c, d, e, f, g, a, b, c', rw]

fixOctave oo ns = map (\(Play t (Note d a o)) -> (Play t (Note d a (o+oo)))) ns

returnToTonic :: Note -> [Event]
returnToTonic (Note C Nat o) = [wn $ Note C Nat o] ++ [rw, rh]
returnToTonic (Note D Nat o) = [wn $ Note D Nat o] ++ fixOctave (o-4) [c] ++ [rw, rh]
returnToTonic (Note E Nat o) = [wn $ Note E Nat o] ++ fixOctave (o-4) [d,c] ++ [rw, rh]
returnToTonic (Note F Nat o) = [wn $ Note F Nat o] ++ fixOctave (o-4) [e,d,c] ++ [rw, rh]
returnToTonic (Note G Nat o) = [wn $ Note G Nat o] ++ fixOctave (o-4) [a,b,c'] ++ [rw, rh]
returnToTonic (Note A Nat o) = [wn $ Note A Nat o] ++ fixOctave (o-4) [b,c'] ++ [rw, rh]
returnToTonic (Note B Nat o) = [wn $ Note B Nat o] ++ fixOctave (o-4) [c'] ++ [rw, rh]

makeMusic :: Int -> [Note] -> [Event]
makeMusic i = concat . take num . insertEvery i scale . map returnToTonic
  where
    num = 35 *(min i 3)
    insertEvery i x xs = [x] ++ take i xs ++ insertEvery i x (drop i xs)


earTraining name i es = do
  g <- newStdGen
  createMidi ("eartraining/et_"++name++".midi") $ music [part g ns ++ [rw]]
  where
    ns = map (\(Play _ n) -> n) es
    randomsChoice g xs = map (xs !!) $ randomRs (0, length xs - 1) g
    part g ns = makeMusic i $ randomsChoice g ns

cd1 = do
  earTraining "01" 1 [c, c']
  earTraining "02" 1 [c, d, b, c']
  earTraining "03" 1 [c, d, e, a, b, c']
  earTraining "04" 1 [c, d, e, f, g, a, b, c']
  earTraining "05" 1 [b_, c, d, e, f, g, a, b, c', d']
  earTraining "06" 1 [a_, b_, c, d, e, f, g, a, b, c', d', e']
  earTraining "07" 1 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "08" 2 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "09" 3 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "10" 4 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "11" 5 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']
  earTraining "12" 999999 [g_, a_, b_, c, d, e, f, g, a, b, c', d', e', f']

cd2 = do
  earTraining "2-01" 5 [c, d, e, f, g, a, b, d, e, f, g, a, b, c']
  earTraining "2-02" 5 [c, d, e, f, g, a, e, f, g, a, b, d, e, f, g, a, b, c']
  earTraining "2-03" 5 [e, f, g, a]
  earTraining "2-04" 5 [d_, b_, d, b, d', b']
  earTraining "2-05" 5 [d, e, f, g, a, b]
  earTraining "2-06" 5 [d_, e_, f_, g_, a_, e_, f_, g_, a_, b_, c, d, e, f, g, a, e, f, g, a, b, c', d', e', f', g', a', e', f', g', a', b']
  earTraining "2-07" 5 [d_, e_, f_, g_, a_, e_, f_, g_, a_, b_, d, e, f, g, a, e, f, g, a, b, d', e', f', g', a', e', f', g', a', b']


main = cd2

