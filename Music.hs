{-|
Module      : Music
Description : Definition of musical structure for use in harmonic analysis
-}

module Music (
  Music(..),
  Time(..),
  Duration(..),
  Part(..),
  SeqEvent(..),
  SeqPart(..),
  Ctx(..),
  Event(..),
  emptyMusic,
  getParts,
  higherPart, lowerPart,
  getEventAt, getEventBefore,
  addEvent
) where
import Note
import Data.List
import Data.Ord
import Data.List.Zipper


-- |Time type
type Time = Rational

-- |Duration type
type Duration = Rational

-- |Part
data Part = Bass | Tenor | Alto | Soprano deriving (Eq, Show)

-- |Context for a music event
data Ctx = Ctx { start :: Time, end :: Time, dur :: Duration, part :: Part } deriving (Eq, Show)

-- |One music event; note or rest, and the surrounding musical context
data Event = Rest Ctx | Play Ctx Note deriving (Eq, Show)

-- |Entire music made up of a list of parts in order from bass to treble
data Music = Music { bass :: Zipper Event, tenor :: Zipper Event, alto :: Zipper Event, soprano :: Zipper Event } deriving (Eq, Show)


-- |One note or rest in a sequence (used for constructing music)
data SeqEvent = SeqRest Duration | SeqPlay Duration Note deriving (Eq, Show)

-- |List of notes and rests in sequence (used for constructing music)
data SeqPart = SeqPart { name :: Part, events :: [SeqEvent] } deriving (Eq, Show)


-- |Empty music with empty parts
emptyMusic :: Music
emptyMusic = Music empty empty empty empty

-- |Get a list with all the inhabited parts
getParts :: Music -> [[Event]]
getParts m = filter (not . null) $ map toList [bass m, tenor m, alto m, soprano m]

-- |Get the next higher Part
higherPart :: Part -> Maybe Part
higherPart Bass = Just Tenor
higherPart Tenor = Just Alto
higherPart Alto = Just Soprano
higherPart Soprano = Nothing

-- |Get the next lower Part
lowerPart :: Part -> Maybe Part
lowerPart Bass = Nothing
lowerPart Tenor = Just Bass
lowerPart Alto = Just Tenor
lowerPart Soprano = Just Alto

-- |Get the event occuring immediately at a given time (if there is one)
getEventAt :: Music -> Part -> Time -> Maybe Event
getEventAt m p t = findFirstEvent (getPart m p) ((<= t).(Music.start).ctx)

-- |Get the event occuring immediately before a given time (if there is one)
getEventBefore :: Music -> Part -> Time -> Maybe Event
getEventBefore m p t = findFirstEvent (getPart m p) ((< t).(Music.start).ctx)

-- |Add a new event to the end of a Part in some Music
addEvent :: Music -> Part -> SeqEvent -> Music
addEvent m p se = setPart m p $ addToPart se $ getPart m p
  where
    addToPart se z = fromList $ ((++) [toEvent se z]) $ toList z
    toEvent (SeqRest d) z = Rest (makeAddEventCtx z p d)
    toEvent (SeqPlay d n) z = Play (makeAddEventCtx z p d) n

getPart :: Music -> Part -> Zipper Event
getPart (Music b _ _ _) Bass = b
getPart (Music _ t _ _) Tenor = t
getPart (Music _ _ a _) Alto = a
getPart (Music _ _ _ s) Soprano = s

setPart :: Music -> Part -> Zipper Event -> Music
setPart (Music _ t a s) Bass p = Music p t a s
setPart (Music b _ a s) Tenor p = Music b p a s
setPart (Music b t _ s) Alto p = Music b t p s
setPart (Music b t a _) Soprano p = Music b t a p

ctx :: Event -> Ctx
ctx (Rest c) = c
ctx (Play c _) = c

makeAddEventCtx :: Zipper Event -> Part -> Duration -> Ctx
makeAddEventCtx z p d = Ctx _start _end _dur _part
  where
    _start = partDuration z
    _end = _start + d
    _dur = _end - _start
    _part = p

partDuration :: Zipper Event -> Duration
partDuration e = if null es then 0 else (Music.end) $ ctx $ last $ toList e
  where
    es = toList e

findFirstEvent :: Zipper Event -> (Event -> Bool) -> Maybe Event
findFirstEvent z _ | endp z = Nothing
findFirstEvent z p | (p.cursor) z = Just $ cursor z
findFirstEvent z p = findFirstEvent (right z) p

