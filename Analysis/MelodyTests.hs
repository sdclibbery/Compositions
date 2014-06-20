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
import Analysis.Result

tests = TestLabel "Melody" $ TestList
    [ testParts
    , testRests
    , testOrder
    , testPCat
    , testRuleH89
    , testRuleH90
    , testRuleH91
    , testRuleH92
    ]

testParts = TestLabel "parts" $ TestList
    [ test [err 0]                  $ part 0
    , test [err 0, err 1]           $ part 0 <> part 1
    ] where
        test e s = show s ~: e ~=? analyse s
        part p = setPart p $ asScore $ scat [c,b]^/4
        err p = Error [p] (0 <-> (1/2)) (Harmony 89) "Dissonance _M7"

testRests = TestLabel "rests" $ TestList
    [ test [err 0, err (3/2)]               $ removeRests $ scat [c,b]^/4 |> rest |> scat [c,b]^/4
    , test [err 0, err (3/2), err 3]    $ removeRests $ scat [c,b]^/4 |> rest |> scat [c,b]^/4 |> rest |> scat [c,b]^/4
    ] where
        test e s = show s ~: e ~=? analyse s
        err t = Error [0] (t >-> (1/2)) (Harmony 89) "Dissonance _M7"

testOrder = TestLabel "order" $ TestList
    [ test [err]                     $ asScore $ mconcat [delay 0 c, delay 1 b]
    , test [err]                     $ asScore $ mconcat [delay 1 b, delay 0 c]
    ] where
        test e s = show s ~: e ~=? analyse s
        err = Error [0] (0 >-> 2) (Harmony 89) "Dissonance _M7"

testPCat = TestLabel "pcat" $ TestList
    [ test [err]                     $ asScore $ pcat [d_,c] |> pcat [e_,b]
    , test [err]                     $ asScore $ pcat [c,d_] |> pcat [b,e_]
    ] where
        test e s = show s ~: e ~=? analyse s
        err = Error [0] (0 >-> 2) (Harmony 89) "Dissonance _M7"

testRuleH89 = TestLabel "ruleH89" $ TestList
    [ test []                                                           [c..c']
    , test [Error [0] (0 <-> (1/2)) (Harmony 89) "Dissonance _M7"]      [c,b]
    , test [Error [0] ((1/2) <-> 1) (Harmony 89) "Dissonance _M7"]      [a_,d,c,b]
    -- Leaving dim/aug for the next rules...
    ] where
        test e s = show s ~: e ~=? analyse (asScore $ scat s^/4)

testRuleH90 = TestLabel "ruleH90" $ TestList
    [ test [Warning [0] (0 <-> (1/2)) (Harmony 90) "Diminished d5"]               [b, f']
    , test [Error [0] (0 <-> (1/2)) (Harmony 90) "Unresolved Diminished d5"]      [b, f', c']
    , test [Error [0] (0 <-> (1/2)) (Harmony 90) "Unresolved Diminished d5"]      [b, f', d']
    , test []                                                                     [b, f', e']
    , test []                                                                     [f', b, c']
    , test [Error [0] (0 <-> (1/2)) (Harmony 90) "Unresolved Diminished -d5"]     [f', b, e']
    , test [Error [0] (0 <-> (1/2)) (Harmony 90) "Outside Diminished d5"]         [b, f', a]
    , test [Error [0] (0 <-> (1/2)) (Harmony 90) "Outside Diminished d5"]         [b, f', g']
    ] where
        test e s = show s ~: e ~=? analyse (asScore $ scat s^/4)

testRuleH91 = TestLabel "ruleH91" $ TestList
    [ test [Error [0] (0 <-> (1/2)) (Harmony 91) "Augmented _A4"]              [f, b]
    , test []                                                                  [c, ds]
    ] where
        test e s = show s ~: e ~=? analyse (asScore $ scat s^/4)

testRuleH92 = TestLabel "ruleH92" $ TestList
    [ test []                                                                      [g, e, e', c']
    , test []                                                                      [e, e']
    , test []                                                                      [g, e, e']
    , test []                                                                      [e, e', c']
    , test [Error [0] ((1/4) <-> (3/4)) (Harmony 92) "Large Interval Approach"]    [d, e, e']
    , test [Error [0] (0 <-> (1/2)) (Harmony 92) "Large Interval Leave"]           [e, e', g']
    , test [Error [0] ((1/4) <-> (3/4)) (Harmony 92) "Large Interval Approach"]    [d, e, e', c']
    , test [Error [0] ((1/4) <-> (3/4)) (Harmony 92) "Large Interval Leave"]       [g, e, e', g']
    ] where
        test e s = show s ~: e ~=? analyse (asScore $ scat s^/4)

-- Helpers

instance Show (Score BasicNote) where
    show s = show $ map (view pitch') $ toList $ mapWithSpan (=:) s

{- Test for an internal method
testIsInInterval = TestLabel "isInInterval" $ TestList
    [ test True                     (nc, ne, nd)
    , test True                     (ne, nc, nd)
    , test False                    (nc, ne, nc)
    , test False                    (nc, nd, ne)
    , test False                    (nd, ne, nc)
    ] where
        test e r@(n1, n2, n3) = show r ~: e ~=? isInInterval n1 n2 n3
        nc = c::Note BasicNote
        nd = d::Note BasicNote
        ne = e::Note BasicNote
-}

