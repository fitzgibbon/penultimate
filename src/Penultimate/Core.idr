module Penultimate.Core

import Data.Fin
import Data.Nat

public export
data Color
  = Named Nat -- 0-15 for 16-color ANSI
  | RGB Nat Nat Nat

public export
Eq Color where
  (Named a) == (Named b) = a == b
  (RGB r1 g1 b1) == (RGB r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2
  _ == _ = False

public export
record Attr where
  constructor MkAttr
  bold : Bool
  underline : Bool
  italic : Bool

public export
Eq Attr where
  (MkAttr b1 u1 i1) == (MkAttr b2 u2 i2) = b1 == b2 && u1 == u2 && i1 == i2

public export
defaultAttr : Attr
defaultAttr = MkAttr False False False

public export
record StyledChar where
  constructor MkStyledChar
  char : Char
  fg : Color
  bg : Color
  attr : Attr

public export
Eq StyledChar where
  (MkStyledChar c1 f1 b1 a1) == (MkStyledChar c2 f2 b2 a2) =
    c1 == c2 && f1 == f2 && b1 == b2 && a1 == a2

public export
defaultStyledChar : StyledChar
defaultStyledChar = MkStyledChar ' ' (Named 7) (Named 0) defaultAttr

public export
interface Surface (0 f : Nat -> Nat -> Type) (0 m : Type -> Type) | f where
  drawRect : {w, h : Nat}
          -> f w h
          -> (x : Fin w) -> (y : Fin h)
          -> (rect_w : Fin (S (minus w (finToNat x))))
          -> (rect_h : Fin (S (minus h (finToNat y))))
          -> StyledChar
          -> m (f w h)

  resize : {w, h : Nat}
        -> f w h
        -> (new_w : Nat) -> (new_h : Nat)
        -> m (f new_w new_h)
