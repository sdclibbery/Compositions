import Test.HUnit
import qualified Diatonic.DiatoneTests
import qualified Diatonic.IntervalTests
import qualified Diatonic.KeysTests
import qualified Diatonic.Analysis.DeconstructTests
import qualified Diatonic.Analysis.MelodyTests
import qualified Diatonic.Analysis.HarmonyTests

{- TODO:
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
