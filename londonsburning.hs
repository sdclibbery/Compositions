-- Londons Burning

import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.Music.Note.Performance
import Euterpea.IO.MIDI.ToMidi

twice :: Music a -> Music a
twice m = timesM 2 m

part1 = twice (twice (d 4 en) :+: twice (g 4 qn))
part2 = twice (twice (a 4 en) :+: twice (b 4 qn))
part3 = twice (twice (d 5 qn) :+: qnr)
part4 = twice (d 5 en :+: c 5 en :+: twice (b 4 qn))

theme = part1 :+: part2 :+: part3 :+: part4

main :: IO ()
main = play theme