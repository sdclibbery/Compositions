import Test.HUnit
import qualified Diatonic.DiatoneTests
import qualified Diatonic.IntervalTests
import qualified Diatonic.Analysis.MelodyTests

main = do
	runTestTT $ TestList
		[ Diatonic.DiatoneTests.tests
		, Diatonic.IntervalTests.tests
		, Diatonic.Analysis.MelodyTests.tests
		]
