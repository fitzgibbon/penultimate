module Penultimate

import Penultimate.Ansi
import Penultimate.Backend
import Penultimate.Capabilities
import Penultimate.Canvas
import public Penultimate.Input
import public Penultimate.InputBackend
import Penultimate.Signal
import Penultimate.Render as Render

public export
record Penultimate (m : Type -> Type) where
  constructor MkPenultimate
  renderCtx : RenderContext m

export
initPenultimate : TerminalBackend m => HasIO m => RenderPolicy -> m (Penultimate m)
initPenultimate policy = do
  ctx <- Render.initRenderContext policy
  _ <- Penultimate.Backend.enableRaw
  writeString hideCursor
  writeString clearScreen
  writeString (cursorTo 1 1)
  flush
  pure (MkPenultimate ctx)

export
shutdown : TerminalBackend m => Penultimate m -> m ()
shutdown _ = do
  _ <- Penultimate.Backend.disableRaw
  writeString resetAttrs
  writeString showCursor
  flush

export
render : TerminalBackend m => HasIO m => Penultimate m -> Canvas -> m ()
render term canvas = Render.renderCanvas term.renderCtx canvas

export
getSize : TerminalBackend m => HasIO m => Penultimate m -> m (Nat, Nat)
getSize term = Render.getSizeCtx term.renderCtx

export
refreshSize : TerminalBackend m => HasIO m => Penultimate m -> m (Nat, Nat)
refreshSize term = do
  Render.refreshSize term.renderCtx
  Render.getSizeCtx term.renderCtx

export
pollEvent : TerminalBackend m => Penultimate m -> m (Maybe Key)
pollEvent _ = Penultimate.InputBackend.readKeyMaybe

export
readKeyEvent : TerminalBackend m => Penultimate m -> m Key
readKeyEvent _ = Penultimate.InputBackend.readKey
