import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
x Try updating to new version of Music Suite
* Experiment with writing lenses for the analysis rules...
 * Also look at 'withContext' as another possible alternative
* Investigate Zippers that can step up and down through parts as well as along melodies
* Harmony tests
-}

main = runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        ]