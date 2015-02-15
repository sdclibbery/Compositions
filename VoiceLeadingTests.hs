import Test.HUnit
import VoiceLeading
import Notes
import Structure

main = runTestTT $ TestList
  [
    -- parts should be in their ranges
    TestLabel "partRange" partRange,
    -- parts mustnt cross
    TestLabel "partsCrossing" partsCrossing
    -- parts mustnt overlap
    -- avoid large leaps
    -- avoid unisons between parts
    -- never have consecutive octaves or fifths (including compounds)
    -- never more than four consecutive of any interval
    -- avoid going to perfect consonance by similar motion (hidden octaves)
  ]


partRange = TestList [
  test Ok                 Treble g,
  test (Error PartRange)  Treble c__,
  test (Error PartRange)  Treble b'',
  test (Error PartRange)  Bass g
  ] where
    test e p s = (show s ++ show p) ~: e ~=? tryAddNote m p s
    m = music [ [c_], [g_], [c], [e] ]

partsCrossing = TestList [
  test Ok                     Alto c,
  test (Error PartsCrossing)  Alto g,
  test (Error PartsCrossing)  Alto g_
  ] where
    test e p s = (show s ++ show p) ~: e ~=? tryAddNote m p s
    m = music [ [c_], [a_, a_], [c], [e, e] ]

