{-|
Module      : Diatonic.Keys
Description : Supply key definitions for standard diatonic musical keys
-}

module Diatonic.Keys where
import Euterpea.Music.Note.Music

-- |Basic Diatonic pitches. The unflattened / unsharpened notes on a normal musical stave.
data BasePitchClass = C' | D' | E' | F' | G' | A' | B' deriving (Show, Eq, Ord, Enum)

-- |Get the unsharpened and unflattened version of a PitchClass
pcToBasePc :: PitchClass -> BasePitchClass
pcToBasePc pc  = case pc of
  Cff  -> C';   Cf  -> C';   C  -> C';   Cs  -> C';   Css  -> C';
  Dff  -> D';   Df  -> D';   D  -> D';   Ds  -> D';   Dss  -> D';
  Eff  -> E';   Ef  -> E';   E  -> E';   Es  -> E';   Ess  -> E';
  Fff  -> F';   Ff  -> F';   F  -> F';   Fs  -> F';   Fss  -> F';
  Gff  -> G';   Gf  -> G';   G  -> G';   Gs  -> G';   Gss  -> G';
  Aff  -> A';   Af  -> A';   A  -> A';   As  -> A';   Ass  -> A';
  Bff  -> B';   Bf  -> B';   B  -> B';   Bs  -> B';   Bss  -> B'

-- |Get a note from a standard key
pitchInKey :: PitchClass -> Mode -> Int -> PitchClass
pitchInKey Cf Major d = [Cf, Df, Ef, Ff, Gf, Af, Bf] !! d
pitchInKey Gf Major d = [Gf, Af, Bf, Cf, Df, Ef, F ] !! d
pitchInKey Df Major d = [Df, Ef, F,  Gf, Af, Bf, C ] !! d
pitchInKey Af Major d = [Af, Bf, C,  Df, Ef, F,  G ] !! d
pitchInKey Ef Major d = [Ef, F,  G,  Af, Bf, C,  D ] !! d
pitchInKey Bf Major d = [Bf, C,  D,  Ef, F,  G,  A ] !! d
pitchInKey F  Major d = [F,  G,  A,  Bf, C,  D,  E ] !! d
pitchInKey C  Major d = [C,  D,  E,  F,  G,  A,  B ] !! d
pitchInKey G  Major d = [G,  A,  B,  C,  D,  E,  Fs] !! d
pitchInKey D  Major d = [D,  E,  Fs, G,  A,  B,  Cs] !! d
pitchInKey A  Major d = [A,  B,  Cs, D,  E,  Fs, Gs] !! d
pitchInKey E  Major d = [E,  Fs, Gs, A,  B,  Cs, Ds] !! d
pitchInKey B  Major d = [B,  Cs, Ds, E,  Fs, Gs, As] !! d
pitchInKey Fs Major d = [Fs, Gs, As, B,  Cs, Ds, Es] !! d
pitchInKey Cs Major d = [Cs, Ds, Es, Fs, Gs, As, Bs] !! d
--
pitchInKey Af Minor d = [Af, Bf, Cf, Df, Ef, Ff, Gf] !! d
pitchInKey Ef Minor d = [Ef, F,  Gf, Af, Bf, Cf, Df] !! d
pitchInKey Bf Minor d = [Bf, C,  Df, Ef, F,  Gf, Af] !! d
pitchInKey F  Minor d = [F,  G,  Af, Bf, C,  Df, Ef] !! d
pitchInKey C  Minor d = [C,  D,  Ef, F,  G,  Af, Bf] !! d
pitchInKey G  Minor d = [G,  A,  Bf, C,  D,  Ef, F ] !! d
pitchInKey D  Minor d = [D,  E,  F,  G,  A,  Bf, C ] !! d
pitchInKey A  Minor d = [A,  B,  C,  D,  E,  F,  G ] !! d
pitchInKey E  Minor d = [E,  Fs, G,  A,  B,  C,  D ] !! d
pitchInKey B  Minor d = [B,  Cs, D,  E,  Fs, G,  A ] !! d
pitchInKey Fs Minor d = [Fs, Gs, A,  B,  Cs, D,  E ] !! d
pitchInKey Cs Minor d = [Cs, Ds, E,  Fs, Gs, A,  B ] !! d
pitchInKey Gs Minor d = [Gs, As, B,  Cs, Ds, E,  Fs] !! d
pitchInKey Ds Minor d = [Ds, Es, Fs, Gs, As, B,  Cs] !! d
pitchInKey As Minor d = [As, Bs, Cs, Ds, Es, Fs, Gs] !! d
