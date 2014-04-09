import Test.HUnit
import qualified Diatonic.DiatoneTests
import qualified Diatonic.IntervalTests
import qualified Diatonic.KeysTests
import qualified Diatonic.Analysis.DeconstructTests
import qualified Diatonic.Analysis.MelodyTests
import qualified Diatonic.Analysis.HarmonyTests

{- TODO:
* Add part odentification
 * In Music as annotation if possible
 * Through into all the analysis
 * And then into all the rules
 * Through into the error/warning reports
* More harmonic analysis rules...
-}

main = do
	runTestTT $ TestList
		[ Diatonic.KeysTests.tests
		, Diatonic.DiatoneTests.tests
		, Diatonic.IntervalTests.tests
		, Diatonic.Analysis.DeconstructTests.tests
		, Diatonic.Analysis.MelodyTests.tests
		, Diatonic.Analysis.HarmonyTests.tests
		]
