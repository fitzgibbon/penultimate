module Terminal.Terminal

import Terminal.Ansi
import Terminal.Capabilities
import Terminal.Frame
import public Terminal.Input
import Terminal.Signal
import Terminal.Render as Render

public export
record Terminal where
  constructor MkTerminal
  renderCtx : RenderContext

export
initTerminal : RenderPolicy -> IO Terminal
initTerminal policy = do
  ctx <- Render.initRenderContext policy
  _ <- collectWinch
  _ <- enableRaw
  putStr hideCursor
  putStr clearScreen
  putStr (cursorTo 1 1)
  pure (MkTerminal ctx)

export
shutdown : Terminal -> IO ()
shutdown _ = do
  _ <- disableRaw
  putStr resetAttrs
  putStr showCursor

export
render : Terminal -> Frame -> IO ()
render term frame = Render.renderFrame term.renderCtx frame

export
getSize : Terminal -> IO (Nat, Nat)
getSize term = Render.getSize term.renderCtx

export
refreshSize : Terminal -> IO (Nat, Nat)
refreshSize term = do
  Render.refreshSize term.renderCtx
  Render.getSize term.renderCtx

export
readKey : Terminal -> IO Key
readKey _ = Terminal.Input.readKey
