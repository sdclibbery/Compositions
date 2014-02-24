-- Londons Burning

import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.Music.Note.Performance
import Euterpea.IO.MIDI.ToMidi
import Diatonic.Diatone
import Diatonic.Analysis.Melody
import Dischord

twice :: Music a -> Music a
twice m = timesM 2 m

part1 = twice (v (-1) en) :+: twice (i 0 qn)
part2 = twice (ii 0 en) :+: twice (iii 0 qn)
part3 = twice (v 0 qn) :+: rest qn
part4 = v 0 en :+: iv 0 en :+: twice (iii 0 qn)

subject = twice part1 :+: twice part2 :+: twice part3 :+: twice part4

intoKey = mDiatoneToPitch G Major 4

music = instrument MusicBox $
  intoKey subject
  :=: delayM (6/4) (intoKey $ diatonicTranspose subject (4))
--  :=: delayM (12/4) (intoKey $ diatonicTranspose subject (5))
--  :=: delayM (6/4) (intoKey $ diatonicTranspose subject (0))

rythm = instrument Percussion $ takeM (8*3/4) $ repeatM $ (perc BassDrum1 qn) :+: twice (perc ClosedHiHat qn)

londonsBurning = music :=: rythm

-- !!! Next: Write an autocorrelator that tries a subject against all correlations

main :: IO ()
main = do
  print $ analyseMelody $ intoKey subject
--  print $ findDischordsM londonsBurning
--  play londonsBurning
