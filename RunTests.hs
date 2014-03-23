import Test.HUnit
import qualified Diatonic.DiatoneTests
import qualified Diatonic.IntervalTests
import qualified Diatonic.KeysTests
import qualified Diatonic.Analysis.MelodyTests

{- TODO:
* Refactor
 * Module to split an arbitrary Music up into 'parts', each being a list of 'sequences'
 * And also correlate the parts against each other
* Move on to Harmonic analysis...
 * Pull Result out as a separate shareable module..?
-}

main = do
	runTestTT $ TestList
		[ Diatonic.KeysTests.tests
		, Diatonic.DiatoneTests.tests
		, Diatonic.IntervalTests.tests
		, Diatonic.Analysis.MelodyTests.tests
		]
