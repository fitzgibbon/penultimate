module Penultimate.Backend

import Data.List
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
interface Monad m => TerminalBackend m where
  initBackend : m ()
  shutdownBackend : m ()
  clearScreen : m ()
  drawTextAt : Nat -> Nat -> RenderStyle -> String -> m ()
  flush : m ()
  readChar : m Char
  pollChar : m (Maybe Char)
  getSize : m (Nat, Nat)
  resizePending : m Bool
  getCapabilities : m Capabilities
  sleep : Int -> m ()
