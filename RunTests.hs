import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
x Rewrite 'analysePart' neater
* Rest of melody rules
 * 92...
* Investigate Zippers that can step up and down through parts as well as along melodies
* Harmony tests 
-}

main = runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        ]
