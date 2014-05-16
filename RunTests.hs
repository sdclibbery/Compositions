import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
* Initial Melody test
 x analyseRule functions should not have to guard against the zipper end; mapZipper should do this...
 * DO move to another zipper now!
 * Move Result etc types out into own file..?
* Rest of melody tests
* Investigate Zippers that can step up and down through parts as well as along melodies
* Harmony tests 

-}

main = do
    runTestTT $ TestList
        [ Analysis.MelodyTests.tests
        ]
