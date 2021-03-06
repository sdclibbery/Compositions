{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Analysis.Melody
Description : Provide a melodic analysis of some Music

Provide analysis of purely sequential (melodic) music.
No analysis of note against note (ie harmonic or contrapuntal) is undertaken in this module.
The analytic rules are according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Analysis.Melody (
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

-- |Analyse a score, applying the melodic analysis rules
-- |Any parts with chords in are discarded. All parts must be monophonic if they are to be analysed.
analyse :: Score BasicNote -> [Result]
analyse = analyseParts . phraseZippers
  where
    analyseParts ps = catMaybes $ Prelude.concat $ mapPairs <$> rules <*> ps
    rules = [ruleH89, ruleH90, ruleH91, ruleH92]

-- Analysis of Music according to Section 89 in Prouts Harmony
-- Any dissonance other than a second is bad
ruleH89 :: Z.Zipper ANote -> Maybe Result
ruleH89 z
  | isStep i       = Nothing
  | isConsonance i = Nothing
  | isDiminished i = Nothing -- Leave for rule 90
  | isAugmented i  = Nothing -- Leave for rule 91
  | otherwise      = Just $ Error [part] s (Harmony 89) $ "Dissonance " ++ show i
    where
      (i, part, s) = getBasicInfo z

-- Analysis of Music according to Section 90 in Prouts Harmony
-- A diminished interval must be resolved correctly
ruleH90 :: Z.Zipper ANote -> Maybe Result
ruleH90 z
  | isDiminished i    = evaluate
  | otherwise         = Nothing
    where
      (i, part, s) = getBasicInfo z
      (l2, l, r, r2) = getNotes z
      evaluate | isNothing r2                    = Just $ Warning [part] s (Harmony 90) $ "Diminished " ++ show i
               | isOutsideInterval l r (fromJust r2) = Just $ Error [part] s (Harmony 90) $ "Outside Diminished " ++ show i
               | isResolution l r (fromJust r2)  = Nothing
               | otherwise                       = Just $ Error [part] s (Harmony 90) $ "Unresolved Diminished " ++ show i
      isResolution a1 a2 a = (==) p (if p2 > p1 then p2 .-^ m2 else p2 .+^ m2) -- Resolution to a diminished is a semitone in each side
        where
          [p1, p2, p] = fmap getPitch [a1, a2, a]

-- Analysis of Music according to Section 91 in Prouts Harmony
-- An augmented interval is always bad (except augmented second)
ruleH91 :: Z.Zipper ANote -> Maybe Result
ruleH91 z
  | isStep i      = Nothing
  | isAugmented i = Just $ Error [part] s (Harmony 91) $ "Augmented " ++ show i
  | otherwise     = Nothing
    where
      (i, part, s) = getBasicInfo z

-- Analysis of Music according to Section 92 in Prouts Harmony
-- A large interval must be approached and left in the opposite direction to the interval
ruleH92 :: Z.Zipper ANote -> Maybe Result
ruleH92 z
  | not isLarge                                  = Nothing
  | isJust l2 && isOutsideInterval l r (fromJust l2) = Just $ Error [part] s (Harmony 92) "Large Interval Approach"
  | isJust r2 && isOutsideInterval l r (fromJust r2) = Just $ Error [part] s (Harmony 92) "Large Interval Leave"
  | otherwise                                    = Nothing
    where
      (i, part, s) = getBasicInfo z
      (l2, l, r, r2) = getNotes z
      isLarge = number i > sixth

-- Helpers

type ANote = (Note BasicNote)

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
    part = view part' r
    s = view onset l <-> view offset r

phraseZippers :: Score BasicNote -> [Z.Zipper ANote]
phraseZippers = splitPhrases . map Z.fromList . monophonic . map (view notes . simultaneous) . extractParts
  where
    splitPhrases = Prelude.concatMap (splitZipper disjoint)
    disjoint t u = view offset t < view onset u || view offset u < view onset t
    monophonic = filter (not . hasChords)
    hasChords = Prelude.any (\n -> lengthOf pitches n > 1)

splitZipper :: (a -> a -> Bool) -> Z.Zipper a -> [Z.Zipper a]
splitZipper p z@(Z.Zip ls rs)
  | Z.endp z = [Z.fromList $ reverse ls]
  | Z.beginp z = splitZipper p $ Z.right z
  | otherwise = if p (head ls) (head rs) then Z.fromList (reverse ls) : splitZipper p (Z.fromList rs) else splitZipper p $ Z.right z

mapPairs :: (Z.Zipper a -> b) -> Z.Zipper a -> [b]
mapPairs f = Z.foldrz foldit []
  where
    foldit z rs = if Z.endp $ Z.right z then rs else f z : rs
