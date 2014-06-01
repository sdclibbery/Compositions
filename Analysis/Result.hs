{-|
Module      : Analysis.Result
Description : Define the result structures for analysis

The analytic rules are according to Ebeneezer Prouts books 'Harmony' and 'Counterpoint'.
-}

module Analysis.Result where
import Music.Prelude.Basic

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
