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
        Source(..),
        Result(..),
        analyse
    ) where
import Music.Prelude.Basic
import Music.Score.Note
import Control.Lens
import Data.Foldable
import Data.Maybe
import Debug.Trace
import qualified Data.List.Zipper as Z


-- |Define the source for an error or warning within Ebeneezer Prouts books
data Source =
    Harmony Int
  | CounterPoint Int
  deriving (Eq, Ord, Show)

-- |One analysis result: an error or warning
data Result =
    Warning [BasicPart] Span Source String
  | Error [BasicPart] Span Source String
  deriving (Eq, Ord, Show)

-- |Analyse a score, applying the melodic analysis rules
analyse :: Score BasicNote -> [Result]
analyse = Prelude.concat . map analysePart . splitPhrases . map zipper . extractParts
  where
    splitPhrases = Prelude.concat . map (splitZipper disjoint)
    disjoint t u = offset t < onset u || offset u < onset t
    analysePart = catMaybes . mapPairs ruleH89

-- Analysis of Music according to Section 89 in Prouts Harmony
-- Any dissonance other than a second is bad
ruleH89 :: Z.Zipper (Note BasicNote) -> Maybe Result
ruleH89 z
  | isConsonance i            = Nothing
  | number i == second        = Nothing
--  | intType i == Diminished   = Nothing -- Leave for rule 90
--  | intType i == Augmented    = Nothing -- Leave for rule 91
  | otherwise                 =  Just $ Error [part] s (Harmony 89) $ "Dissonance " ++ show i
    where
        l = Z.cursor z
        r = Z.cursor $ Z.right z
        i = (__getPitch r) .-. (__getPitch l)
        part = getPart $ getNoteValue r
        s = onset l <-> offset r

-- Helpers

zipper :: Score BasicNote -> Z.Zipper (Note BasicNote)
zipper = Z.fromList . toList . mapWithSpan (=:)

splitZipper :: (a -> a -> Bool) -> Z.Zipper a -> [Z.Zipper a]
splitZipper p z@(Z.Zip ls rs)
  | Z.endp z = [Z.fromList $ reverse ls]
  | Z.beginp z = splitZipper p $ Z.right z
  | otherwise = if p (head ls) (head rs) then (Z.fromList $ reverse ls) : splitZipper p (Z.fromList rs) else splitZipper p $ Z.right z

mapPairs :: (Z.Zipper a -> b) -> Z.Zipper a -> [b]
mapPairs f = Z.foldrz foldit []
  where
    foldit z rs = if Z.endp $ Z.right z then rs else (f z) : rs

instance HasGetPitch (ChordT BasicPitch) where
    __getPitch c = case getChord c of
      (p:[]) -> p
      otherwise -> error "Chords not supported for melodic analysis"
