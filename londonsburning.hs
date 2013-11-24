-- Londons Burning

import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.Music.Note.Performance
import Euterpea.IO.MIDI.ToMidi
import Interval

twice :: Music a -> Music a
twice m = timesM 2 m

part1 = twice (v (-1) en) :+: twice (i 0 qn)
part2 = twice (ii 0 en) :+: twice (iii 0 qn)
part3 = twice (v 0 qn) :+: rest qn
part4 = v 0 en :+: iv 0 en :+: twice (iii 0 qn)

theme = {-twice $-} twice part1 :+: twice part2 :+: twice part3 :+: twice part4

intoKey = mIntToPitch G Major 4

londonsBurning = instrument MusicBox $
	intoKey theme
	:=: delayM (6/4) (intoKey (mTranspose theme (-3)))

main :: IO ()
main = play londonsBurning
