module Penultimate

import Penultimate.Ansi
import Penultimate.Capabilities
import Penultimate.Canvas
import public Penultimate.Input
import Penultimate.Signal
import Penultimate.Render as Render

public export
record Penultimate where
  constructor MkPenultimate
  renderCtx : RenderContext

export
initPenultimate : RenderPolicy -> IO Penultimate
initPenultimate policy = do
  ctx <- Render.initRenderContext policy
  _ <- collectWinch
  _ <- enableRaw
  putStr hideCursor
  putStr clearScreen
  putStr (cursorTo 1 1)
  pure (MkPenultimate ctx)

export
shutdown : Penultimate -> IO ()
shutdown _ = do
  _ <- disableRaw
  putStr resetAttrs
  putStr showCursor

export
render : Penultimate -> Canvas -> IO ()
render term canvas = Render.renderCanvas term.renderCtx canvas

export
getSize : Penultimate -> IO (Nat, Nat)
getSize term = Render.getSize term.renderCtx

export
refreshSize : Penultimate -> IO (Nat, Nat)
refreshSize term = do
  Render.refreshSize term.renderCtx
  Render.getSize term.renderCtx

export
readKey : Penultimate -> IO Key
readKey _ = Penultimate.Input.readKey

export
pollEvent : Penultimate -> IO (Maybe Key)
pollEvent _ = Penultimate.Input.readKeyMaybe
