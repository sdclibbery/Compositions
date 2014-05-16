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
    analysePart = catMaybes . mapZipper ruleH89

-- Analysis of Music according to Section 89 in Prouts Harmony
-- Any dissonance other than a second is bad
ruleH89 :: Zipper (Note BasicNote) -> Maybe Result
ruleH89 (_, []) = Nothing
ruleH89 (l:ls, r:rs)
  | isConsonance i            = Nothing
  | number i == second        = Nothing
--  | intType i == Diminished   = Nothing -- Leave for rule S90
--  | intType i == Augmented    = Nothing -- Leave for rule S91
  | otherwise                 =  Just $ Error [part] s (Harmony 89) $ "Dissonance " ++ show i
    where
        i = (__getPitch l) .-. (__getPitch r)
        part = getPart $ getNoteValue l
        s = onset r <-> offset l

-- Helpers

zipper :: Score BasicNote -> Zipper (Note BasicNote)
zipper s =  (toList $ mapWithSpan (=:) s, [])

type Zipper a = ([a], [a]) -- (next, previous)

next :: Zipper a -> Maybe (Zipper a)
next ([], _) = Nothing
next (l:ls, rs) = Just (ls, l:rs)

splitZipper :: (a -> a -> Bool) -> Zipper a -> [Zipper a]
splitZipper _ ([], rs) = [(reverse rs, [])]
splitZipper p z@(_, []) = splitZipper p $ fromJust $ next z
splitZipper p z@(l:ls, r:rs) = if p l r then (reverse (r:rs), []) : splitZipper p (l:ls, []) else splitZipper p $ fromJust $ next z

mapZipper :: (Zipper a -> b) -> Zipper a -> [b]
mapZipper f z = let n = next z in
  if isNothing n then []
  else (f z) : (mapZipper f $ fromJust n)        

instance HasGetPitch (ChordT BasicPitch) where
    __getPitch c = case getChord c of
      (p:[]) -> p
      otherwise -> error "Chords not supported for melodic analysis"
