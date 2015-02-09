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

-- |Identify one part in harmonic music
data Part = Bass | Tenor | Alto | Treble deriving (Show, Eq, Ord, Enum)

-- |A thing that happens at a certain time
data Temporal a = Temporal Part Time a deriving (Show, Eq, Ord)

-- |Indicates whether two sequences have the same arrangement of events (but not necessarily the same event contents)
data Fit = Exact | Mismatch deriving (Show, Eq)


-- |An event that lasts for a certain duration
type Event a = (Dur, a)

-- |A musical fragment: sequential music without any parallel constructors
type Monophonic a = Temporal (Music a)

-- |Line of musical primitives
type Line a = Temporal [Primitive a]

-- |A sequence of events starting at a certain time
type Sequence a = Temporal [Event a]

-- |A Musical part
type MusicPart a = (Part, Music a)


-- |Convert a general Music into a list of Sequences, which are monophonic and continuous
musicToSequences :: Music a -> [Sequence a]
musicToSequences = processSequences . monophonics 0 Alto

-- |Convert a set of musical parts into a set of monophonic sequences suitable for analysis.
-- |The lowest part must be Bass and the top most Treble, not matter how many parts there are.
partsToSequences :: [MusicPart a] -> [Sequence a]
partsToSequences = concat . map processSequences . map (uncurry (monophonics 0))

-- |Correlate two sequences, giving a list of the overlapping items and when they occur
correlateSequences :: (Sequence a, Sequence a) -> (Part, Part, [(Time, a, a)], Fit)
correlateSequences (Temporal p1 _ [], Temporal p2 _ []) = (p1, p2, [], Exact)
correlateSequences (Temporal p1 _ [], Temporal p2 _ _) = (p1, p2, [], Mismatch)
correlateSequences (Temporal p1 _ _, Temporal p2 _ []) = (p1, p2, [], Mismatch)
correlateSequences (Temporal p1 t1 (e1:es1), Temporal p2 t2 (e2:es2)) = if t1 < t2 then correlate p1 t1 e1 es1 p2 t2 e2 es2 else correlate p2 t2 e2 es2 p1 t1 e1 es1
    where
        correlate pl tl (dl, xl) esl ph th eh@(dh, xh) esh = correlation `combine` correlateSequences (truncatedlo, truncatedhi)
            where
                correlation = if overlap then (pl, ph, [(th, xl, xh)], fit) else (pl, ph, [], Mismatch)
                overlap = tl + dl > th
                truncatedlo = if tl + dl <= th + dh then Temporal pl (tl+dl) esl else Temporal pl th ((dl-th+tl, xl):esl)
                truncatedhi = if tl + dl < th + dh then Temporal ph th (eh:esh) else Temporal ph (th+dh) esh
                fit = if t1 == t2 && dl == dh then Exact else Mismatch
                combine (p1, p2, es1, o1) (_, _, es2, o2) = (p1, p2, es1 ++ es2, if o1 == Mismatch then o1 else o2)


-- Process monophonic music sections into Sequences (lists of notes)
processSequences :: [Monophonic a] -> [Sequence a]
processSequences = map lineToSequence . concat . map splitOnRests . map flatten

-- Split some music into a list of monophonic sections
monophonics :: Time -> Part -> Music a -> [Monophonic a]
monophonics t p (m1 :=: m2) = monophonics t p m1 ++ monophonics t p m2
monophonics t p (m1 :+: m2) = monophonics t p m1 `combine` monophonics (t + dur m1) p m2
  where
    combine [(Temporal p1 t1 m1)] [(Temporal p2 t2 m2)] = [(Temporal p1 t1 $ m1 :+: m2)]
    combine s1 s2 = s1 ++ s2
monophonics t p m = [(Temporal p t m)]

-- Flatten a Monophonic into a Line: a list of music primitives
flatten :: Monophonic a -> Line a
flatten (Temporal pt t (Prim p)) = (Temporal pt t [p])
flatten (Temporal pt t (m1 :+: m2)) = (flatten (Temporal pt t m1)) `concatLines` (flatten (Temporal pt t m2))
	where
		concatLines l1 l2 = (Temporal pt t $ prims l1 ++ prims l2)
		prims (Temporal _ _ ps) = ps
flatten (Temporal pt t (m1 :=: m2)) = error "Oops, flatten can only be used with Music that is already monophonic"

-- Split a Line up on rests
splitOnRests :: Line a -> [Line a]
splitOnRests (Temporal pt t prims) = case span (not.isRest) prims of
    ([], []) -> []
    (ps, []) -> [(Temporal pt t ps)]
    ([], pps) -> splitOnRests (remainder t pps)
    (ps, pps) -> [(Temporal pt t ps)] ++ splitOnRests (remainder (t + durNotes ps) pps)
  where
    remainder t pps = Temporal pt (t + durRest (head pps)) $ tail pps
    durNotes = foldr (\(Note d _) a -> a + d) 0
    durRest (Rest d) = d
    isRest (Rest _) = True
    isRest _ = False

-- Transform a Line with no rests in into a Sequence of notes
lineToSequence :: Line a -> Sequence a
lineToSequence (Temporal pt t ps) = (Temporal pt t $ map (\(Note d n) -> (d, n)) ps)
