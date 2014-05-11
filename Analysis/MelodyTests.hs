{-# LANGUAGE FlexibleInstances #-}

module Analysis.MelodyTests (
        tests
    ) where
import Test.HUnit
import Analysis.Melody
import Music.Prelude.Basic
import Control.Lens
import Data.Foldable
import Data.Ratio

tests = TestLabel "Melody" $ TestList
    [ testParts
    , testRuleH89
    ]

testParts = TestLabel "parts" $ TestList
    [ test [err 0]                  $ part 0
    , test [err 0, err 1]           $ part 0 <> part 1
    ] where
        test e s = (show s) ~: e ~=? (analyse s)
        part p = setPart p $ asScore $ scat [c,b]^/4
        err p = Error [p] (1/4) (Harmony 89) "Dissonance _M7"

testRuleH89 = TestLabel "ruleH89" $ TestList
    [ test []                                                   $ [c..c']
    , test [Error [0] (1/4) (Harmony 89) "Dissonance _M7"]      $ [c,b]
    , test [Error [0] (3/4) (Harmony 89) "Dissonance _M7"]      $ [a_,b_,c,b]
    -- Leaving dim/aug for the next rule...
    ] where
        test e s = (show s) ~: e ~=? (analyse $ asScore $ scat s^/4)

-- Helpers

instance Show (Score BasicNote) where
    show s = show $ map (view pitch') $ toList s

