import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
* Initial Melody test
 * Use a zipper from a lib
 * Look at putting the errors back into the score...
 * Move Result etc types out into own file..?
 * Tests with rests
* Rest of melody tests
* Investigate Zippers that can step up and down through parts as well as along melodies
* Harmony tests 

-}

main = do
    runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        ]
