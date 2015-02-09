import Test.HUnit
import qualified Analysis.MelodyTests
import qualified Analysis.HarmonyTests

{- TODO:
* Harmony tests
 * Section 96: consecutive unisons
-}

main = runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        , Analysis.HarmonyTests.tests
        ]