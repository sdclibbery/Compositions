import Test.HUnit
import VoiceLeading
import Notes
import Structure

main = runTestTT $ TestList
  [
    TestLabel "partRange" partRange
  ]


partRange = TestList [
  test Ok                 Treble c{-,
  test (Error PartRange)  Treble c__-}
  ] where
    test e p s = (show s ++ show p) ~: e ~=? tryAddNote m p s
    m = music [ [c_], [g_], [c], [e] ]

