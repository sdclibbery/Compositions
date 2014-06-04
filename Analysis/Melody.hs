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
import Music.Prelude.Basic
import Music.Score.Note
import Control.Lens
import Data.Foldable
import Data.Maybe
import qualified Data.List.Zipper as Z
import Data.Ord
import Analysis.Result
import Control.Applicative

-- |Analyse a score, applying the melodic analysis rules
analyse :: Score BasicNote -> [Result]
analyse = analyseParts . splitPhrases . map zipper . splitVoices
  where
    splitVoices = Prelude.concatMap separateVoices . extractParts
    splitPhrases = Prelude.concatMap (splitZipper disjoint)
    disjoint t u = offset t < onset u || offset u < onset t
    analyseParts ps = catMaybes $ Prelude.concat $ mapPairs <$> rules <*> ps
    rules = [ruleH89, ruleH90]

-- Analysis of Music according to Section 89 in Prouts Harmony
-- Any dissonance other than a second is bad
ruleH89 :: Z.Zipper (Note BasicNote) -> Maybe Result
ruleH89 z
  | isStep i                  = Nothing
  | isConsonance i            = Nothing
  | isDiminished i            = Nothing -- Leave for rule 90
  | isAugmented i             = Nothing -- Leave for rule 91
  | otherwise                 = Just $ Error [part] s (Harmony 89) $ "Dissonance " ++ show i
    where
      (i, part, s) = getBasicInfo z

-- Analysis of Music according to Section 90 in Prouts Harmony
-- A diminished interval must be resolved correctly
ruleH90 :: Z.Zipper (Note BasicNote) -> Maybe Result
ruleH90 z
  | isDiminished i            = Just $ Error [part] s (Harmony 90) $ "Dissonance " ++ show i
  | otherwise                 = Nothing
    where
      (i, part, s) = getBasicInfo z

-- Helpers

getBasicInfo z = (i, part, s)
    where
        l = Z.cursor z
        r = Z.cursor $ Z.right z
        i = __getPitch r .-. __getPitch l
        part = getPart $ getNoteValue r
        s = onset l <-> offset r

zipper :: Score BasicNote -> Z.Zipper (Note BasicNote)
zipper = Z.fromList . toList . mapWithSpan (=:)

splitZipper :: (a -> a -> Bool) -> Z.Zipper a -> [Z.Zipper a]
splitZipper p z@(Z.Zip ls rs)
  | Z.endp z = [Z.fromList $ reverse ls]
  | Z.beginp z = splitZipper p $ Z.right z
  | otherwise = if p (head ls) (head rs) then Z.fromList (reverse ls) : splitZipper p (Z.fromList rs) else splitZipper p $ Z.right z

mapPairs :: (Z.Zipper a -> b) -> Z.Zipper a -> [b]
mapPairs f = Z.foldrz foldit []
  where
    foldit z rs = if Z.endp $ Z.right z then rs else f z : rs

instance HasGetPitch (ChordT BasicPitch) where
    __getPitch c = case getChord c of
      (p:[]) -> p
      otherwise -> error "Chords not supported for melodic analysis"
