import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
x Rewrite 'analysePart' neater
* Rest of melody rules
 * 90
  * Rest of tests plus full implementation
* Investigate Zippers that can step up and down through parts as well as along melodies
* Harmony tests 

-}

main = do
    runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        ]
