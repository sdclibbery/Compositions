{-|
Module      : Diatonic.Analysis.Deconstruct
Description : Deconstruct general music in various ways suitable for further analysis
-}

module Diatonic.Analysis.Deconstruct where
import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Diatonic.Interval


-- |Define time within some music
type Time = Rational

-- |A thing that happens at a certain time
data Temporal a = Temporal Time a deriving (Show, Eq, Ord)


-- |An event that lasts for a certain duration
type Event a = (Dur, a)

-- |A musical part: sequential music without any parallel constructors
type Part a = Temporal (Music a)

-- |Line of musical primitives
type Line a = Temporal [Primitive a]

-- |A sequence of events starting at a certain time
type Sequence a = Temporal [Event a]


-- Split some music into a list of monophonic parts
monophonicParts :: Time -> Music a -> [Part a]
monophonicParts t (m1 :=: m2) = monophonicParts t m1 ++ monophonicParts t m2
monophonicParts t (m1 :+: m2) = monophonicParts t m1 `combine` monophonicParts (t + dur m1) m2
  where
    combine [(Temporal t1 m1)] [(Temporal t2 m2)] = [(Temporal t1 $ m1 :+: m2)]
    combine s1 s2 = s1 ++ s2
monophonicParts t m = [(Temporal t m)]

-- Flatten a Part into a Line: a list of music primitives
flatten :: Part a -> Line a
flatten (Temporal t (Prim p)) = (Temporal t [p])
flatten (Temporal t (m1 :+: m2)) = (flatten (Temporal t m1)) `concatLines` (flatten (Temporal t m2))
	where
		concatLines l1 l2 = (Temporal t $ prims l1 ++ prims l2)
		prims (Temporal _ ps) = ps
flatten (Temporal t (m1 :=: m2)) = error "Oops, flatten can only be used with Music that is already monophonic"

-- Split a Line up on rests
splitOnRests :: Line a -> [Line a]
splitOnRests (Temporal t prims) = case span (not.isRest) prims of
    ([], []) -> []
    (ps, []) -> [(Temporal t ps)]
    ([], pps) -> splitOnRests (remainder t pps)
    (ps, pps) -> [(Temporal t ps)] ++ splitOnRests (remainder (t + durNotes ps) pps)
  where
    remainder t pps = Temporal (t + durRest (head pps)) $ tail pps
    durNotes = foldr (\(Note d _) a -> a + d) 0
    durRest (Rest d) = d
    isRest (Rest _) = True
    isRest _ = False

-- Transform a Line with no rests in into a Sequence of notes
lineToSequence :: Line a -> Sequence a
lineToSequence (Temporal t ps) = (Temporal t $ map (\(Note d n) -> (d, n)) ps)

-- |Convert a general Music into a list of Sequences, which are monophonic and continuous
musicToSequences :: Music a -> [Sequence a]
musicToSequences = map lineToSequence . concat . map splitOnRests . map flatten . monophonicParts 0
