-- Simple dischord analysis for music
-- Find semitone and tone clashes within a Music

module Analysis.Dischord (Dischord, findDischords, findDischordsM) where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.Music.Note.Performance

data Dischord = MkDischord { dischord :: Int, dischordStart :: PTime, dischordDur :: Dur } deriving (Eq)
instance Show Dischord where
  show d
    | dischord d == 1 = "Semi at beat " ++ (showWhen d) ++ "\n"
    | otherwise = "Tone at beat " ++ (showWhen d) ++ "\n"
    | otherwise = "Unknown dischord!\n"
    where showWhen d = (show $ startTime d) ++ " for " ++ (show $ dischordDur d)
          startTime d = 4 * fromRational (dischordStart d) -- Start time in qn beats

calcDiscordance :: AbsPitch -> AbsPitch -> Int
calcDiscordance a1 a2 = case d of 1 -> 1 -- Semitone dischord
                                  11 -> 1
                                  2 -> 2 -- Tone dischord
                                  10 -> 2
                                  _ -> 0 -- Nothing else matters
  where d = abs $ (a1 `mod` 12) - (a2 `mod` 12)

findDischordsForOne :: Event -> [Event] -> [Dischord]
findDischordsForOne e1 (e:es)
  | overlap > 0 && discordance > 0 = (MkDischord discordance overlapStart overlap) : findDischordsForOne e1 es
  | overlap > 0 = (findDischordsForOne e1 es)
  | otherwise = []
  where overlapStart = max (eTime e1) (eTime e)
        end1 = (eTime e1) + (eDur e1)
        end = (eTime e) + (eDur e)
        overlapEnd = min end1 end
        overlap = overlapEnd - overlapStart
        discordance = calcDiscordance (ePitch e1) (ePitch e)
findDischordsForOne e1 [] = []

findDischords :: [Event] -> [Dischord]
findDischords (e:es)
  | (eInst e) == Percussion && (eVol e) > 0 = findDischords es
  | otherwise = (findDischordsForOne e es) ++ (findDischords es)
findDischords [] = []

findDischordsM :: Music Pitch -> [Dischord]
findDischordsM m = findDischords $ toPerf defPMap (defCon {cDur = 1}) m
