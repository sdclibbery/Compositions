import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
* Try updating to new vwersion of Music Suite
 ? extractParts..?
* Investigate Zippers that can step up and down through parts as well as along melodies
* Harmony tests
-}

main = runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        ]
