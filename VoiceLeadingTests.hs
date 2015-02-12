import Test.HUnit
import VoiceLeading
import Notes
import Structure

main = runTestTT $ TestList
  [
    TestLabel "partRange" partRange
  ]


partRange = TestList [
  test Ok                 Treble g,
  test (Error PartRange)  Treble c__,
  test (Error PartRange)  Treble b'',
  test (Error PartRange)  Bass g
  ] where
    test e p s = (show s ++ show p) ~: e ~=? tryAddNote m p s
    m = music [ [c_], [g_], [c], [e] ]

