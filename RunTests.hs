import Test.HUnit
import qualified Diatonic.DiatoneTests
import qualified Diatonic.IntervalTests

main = do
	runTestTT $ TestList
		[ Diatonic.DiatoneTests.tests
		, Diatonic.IntervalTests.tests
		]
