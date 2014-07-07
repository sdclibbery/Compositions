{-# LANGUAGE FlexibleInstances #-}

module Analysis.HarmonyTests (
        tests
    ) where
import Test.HUnit
import Analysis.Harmony
import Music.Prelude.Basic
import Control.Lens hiding ((|>))
import Data.Foldable
import Data.Ratio
import Analysis.Result

tests = TestLabel "Harmony" $ TestList
    [ testRuleH96
    ]

testRuleH96 = TestLabel "ruleH96" $ TestList
    [ test []                                       (scat [c, d])
    , test []                                       (scat [c, d] <> scat [d, e])
    , test []                                       (scat [c, d] <> scat [c, e])
    , test []                                       (scat [c, d] <> scat [d, d])
    , test [err]                                    (scat [c, d] <> scat [c, d])
    , test []                                       (scat [c, d] <> scat [b, c, d])
    ] where
        test e s = show s ~: e ~=? analyse (asScore s)
        err = Error [0, 1] (0 <-> 2) (Harmony 96) "Consecutive unisons"

-- Helpers

instance Show (Score BasicNote) where
    show s = show $ map (! 0) $ toListOf pitches s
