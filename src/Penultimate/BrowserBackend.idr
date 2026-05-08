module Penultimate.BrowserBackend

import Penultimate.Backend
import Penultimate.Capabilities
import Penultimate.Ansi

%foreign "javascript:lambda: (s) => window.penultimate_writeString(s)"
prim__writeString : String -> PrimIO ()

%foreign "javascript:lambda: () => window.penultimate_readChar()"
prim__readChar : PrimIO Char

%foreign "javascript:lambda: () => { let c = window.penultimate_pollChar(); return c === null ? 0 : c.charCodeAt(0); }"
prim__pollCharInt : PrimIO Int

safeWriteString : String -> IO ()
safeWriteString s = primIO (prim__writeString s)

safeReadChar : IO Char
safeReadChar = primIO prim__readChar

safePollChar : IO (Maybe Char)
safePollChar = do
  code <- primIO prim__pollCharInt
  if code == 0 then pure Nothing else pure (Just (cast code))

export
TerminalBackend IO where
  initBackend = do
    safeWriteString "\x1b[?25l"
    safeWriteString "\x1b[2J"
    safeWriteString "\x1b[1;1H"
  shutdownBackend = do
    safeWriteString "\x1b[0m"
    safeWriteString "\x1b[?25h"
  clearScreen = safeWriteString "\x1b[2J"
  drawTextAt r c style text = safeWriteString ("\x1b[" ++ show r ++ ";" ++ show c ++ "H" ++ styleSeq style ++ text)
  flush = pure ()
  readChar = safeReadChar
  pollChar = safePollChar
  getSize = pure (24, 80)
  resizePending = pure False
  getCapabilities = pure (MkCapabilities True True True)
  sleep ms = pure ()
