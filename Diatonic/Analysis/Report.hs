{-|
Module      : Diatonic.Analysis.Report
Description : Define the reporting structures for Musical analysis
-}

module Diatonic.Analysis.Report where
import Diatonic.Analysis.Deconstruct

-- |Define the source for an error or warning within Prouts books
data Source = Harmony Int | CounterPoint Int deriving (Eq, Ord, Show)

-- |One analysis result: an error or warning
data Result = Warning Time Source String | Error Time Source String deriving (Eq, Ord, Show)
