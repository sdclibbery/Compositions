{-# LANGUAGE FlexibleInstances #-}

module Analysis.MelodyTests (
        tests
    ) where
import Test.HUnit
import Analysis.Melody
import Music.Prelude.Basic
import Control.Lens hiding ((|>))
import Data.Foldable
import Data.Ratio

tests = TestLabel "Melody" $ TestList
    [ testParts
    , testRests
    , testRuleH89
    ]

testParts = TestLabel "parts" $ TestList
    [ test [err 0]                  $ part 0
    , test [err 0, err 1]           $ part 0 <> part 1
    ] where
        test e s = (show s) ~: e ~=? (analyse s)
        part p = setPart p $ asScore $ scat [c,b]^/4
        err p = Error [p] (0 <-> (1/2)) (Harmony 89) "Dissonance _M7"

testRests = TestLabel "rests" $ TestList
    [ test [err 0, err (3/2)]               $ removeRests $ scat [c,b]^/4 |> rest |> scat [c,b]^/4
    , test [err 0, err (3/2), err 3]    $ removeRests $ scat [c,b]^/4 |> rest |> scat [c,b]^/4 |> rest |> scat [c,b]^/4
    ] where
        test e s = (show s) ~: e ~=? (analyse s)
        err t = Error [0] (t >-> (1/2)) (Harmony 89) "Dissonance _M7"

testRuleH89 = TestLabel "ruleH89" $ TestList
    [ test []                                                           $ [c..c']
    , test [Error [0] (0 <-> (1/2)) (Harmony 89) "Dissonance _M7"]      $ [c,b]
    , test [Error [0] ((1/2) <-> 1) (Harmony 89) "Dissonance _M7"]      $ [a_,b_,c,b]
    -- Leaving dim/aug for the next rule...
    ] where
        test e s = (show s) ~: e ~=? (analyse $ asScore $ scat s^/4)

-- Helpers

instance Show (Score BasicNote) where
    show s = show $ map (view pitch') $ toList s

