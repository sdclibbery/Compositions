import Test.HUnit
import qualified Analysis.MelodyTests

{- TODO:
-}

main = do
	runTestTT $ TestList
		[ Analysis.MelodyTests.tests
		]
