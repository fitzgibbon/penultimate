module Penultimate.Backend

import Data.Fin
import Data.List
import Data.Vect
import Data.So
import Penultimate.Capabilities
import Penultimate.Color
import Penultimate.Attr

public export
record RenderStyle where
  constructor MkRenderStyle
  fgStyle : Color
  bgStyle : Color
  attrsStyle : Attrs

export
Eq RenderStyle where
  (MkRenderStyle fg1 bg1 attrs1) == (MkRenderStyle fg2 bg2 attrs2) =
    fg1 == fg2 && bg1 == bg2 && attrs1 == attrs2

public export
record StyledChar where
  constructor MkStyledChar
  char : Char
  style : RenderStyle

export
Eq StyledChar where
  (MkStyledChar c1 s1) == (MkStyledChar c2 s2) = c1 == c2 && s1 == s2

public export
interface Monad m => TerminalBackend m where
  initBackend : m ()
  shutdownBackend : m ()
  clearScreen : m ()

  -- The core type-safe semantic drawing primitives
  drawChar : {w, h : Nat} -> Fin h -> Fin w -> StyledChar -> m ()
  drawLine : {w, h : Nat} -> Fin h -> (c : Fin w) -> {len : Nat} -> Vect len StyledChar -> So (finToNat c + len <= w) -> m ()
  drawRect : {w, h : Nat} -> (r : Fin h) -> (c : Fin w) -> {rw, rh : Nat} -> Vect rh (Vect rw StyledChar) -> So (finToNat c + rw <= w) -> So (finToNat r + rh <= h) -> m ()

  flush : m ()
  readChar : m Char
  pollChar : m (Maybe Char)
  getSize : m (Nat, Nat)
  resizePending : m Bool
  getCapabilities : m Capabilities
  sleep : Int -> m ()
