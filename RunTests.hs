import Diatonic.DiatoneTests as DiatoneTests
import Diatonic.IntervalTests as IntervalTests

-- Run all Diatonic test suites

main = do
	DiatoneTests.run
	IntervalTests.run
