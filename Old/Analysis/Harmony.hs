{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Analysis.Harmony
Description : Provide a harmonic analysis of some Music

Provide analysis of music with harmonies in multiple parts.
The analytic rules are according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Analysis.Harmony (
        analyse
    ) where
import Music.Prelude.Basic as M
import Music.Score as S
import Control.Lens
import Data.Foldable
import Data.Maybe
import qualified Data.List.Zipper as Z
import Data.Ord
import Analysis.Result
import Control.Applicative


-- ??How much gubbins in here (or Melody?) can be replaced by 'phrases' and/or 'mvoices'??
 -- Would need to use AFTER extract parts
 -- The question will be, can we still get the part and span info after using phrases/voices..??
-- Current datastructure is (see HarmonyZipper) rests are (Note Nothings), and we have an 'event' zipper containing part zippers...
 -- Does this make sense? Do we want a top level zipper across the parts?
-- Need to fill out the functions immediately below
 -- harmonyZipper needs to wrap the part zippers into a harmony zipper
 -- analyseParts will need to find all combinations of parts in the HarmonyZipper and call each rule on each
-- partZippers needs to sort the parts from bass to treble...
-- REALLY need tests with rests in :-)
  -- partZippers will need to find disjoint, and instead of splitting, it'll have to insert rests
   -- ?? Is there already a function for this somewhere?



-- |Analyse a score, applying the harmonic analysis rules
-- |Any parts with chords in are discarded; all parts must be monophonic if they are to be analysed.
analyse :: Score BasicNote -> [Result]
analyse = analyseParts . {-harmonyZippers . -}partZippers
  where
    analyseParts ps = [] -- ? -- Step through in time through every event, and then across all part combinations
    rules = [] -- ruleH89]

type HarmonyZipper = Z.Zipper (Z.Zipper ANote)

-- Convert a list of phrases into a 
--harmonyZipper :: [Z.Zipper ANote] -> HarmonyZipper
--harmonyZipper ps = ?

{-
-- Analysis of Music according to Section 96 in Prouts Harmony
-- Consecutive unisons are bad
ruleH96 :: HarmonyZipper -> HarmonyZipper -> Maybe Result
ruleH96 z
  | isStep i       = Nothing
  | isConsonance i = Nothing
  | isDiminished i = Nothing -- Leave for rule 90
  | isAugmented i  = Nothing -- Leave for rule 91
  | otherwise      = Just $ Error [part] s (Harmony 89) $ "Dissonance " ++ show i
    where
      (i, part, s) = getBasicInfo z
-}

-- Helpers

type ANote = Note (Maybe BasicNote)

getPitch :: ANote -> M.Pitch
getPitch x = (! 0) $ x ^?! pitches

isOutsideInterval :: ANote -> ANote -> ANote -> Bool
isOutsideInterval a1 a2 a = p <= min p1 p2 || p >= max p1 p2
  where
    [p1, p2, p] = fmap getPitch [a1, a2, a]

getNotes :: Z.Zipper ANote -> (Maybe ANote, ANote, ANote, Maybe ANote)
getNotes z = (l2, l, r, r2)
  where
        l2 = if Z.beginp z then Nothing else Z.safeCursor $ Z.left z
        l = Z.cursor z
        r = Z.cursor $ Z.right z
        r2 = Z.safeCursor $ Z.right $ Z.right z

getBasicInfo :: Z.Zipper ANote -> (M.Interval, BasicPart, Span)
getBasicInfo z = (i, part, s)
  where
    (_, l, r, _) = getNotes z
    i = getPitch r .-. getPitch l
    part = view part' (fromJust $ view noteValue r)
    s = view onset l <-> view offset r

partZippers :: Score BasicNote -> [Z.Zipper ANote]
partZippers = map (Z.fromList . noteToMaybe) . monophonic . map (view notes . simultaneous) . extractParts
  where
    noteToMaybe = map (over noteValue Just)
    monophonic = filter (not . hasChords)
    hasChords = Prelude.any (\n -> lengthOf pitches n > 1)

mapPairs :: (Z.Zipper a -> b) -> Z.Zipper a -> [b]
mapPairs f = Z.foldrz foldit []
  where
    foldit z rs = if Z.endp $ Z.right z then rs else f z : rs
