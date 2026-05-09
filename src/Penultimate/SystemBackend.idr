module Penultimate.SystemBackend
import Data.Fin
import Data.Vect
import Data.So

import Penultimate.Backend
import Penultimate.Capabilities
import Penultimate.Input
import Penultimate.Signal
import Penultimate.Render
import Penultimate.Ansi
import System
import System.File
import Data.String
import Data.Maybe

%foreign "C:usleep, libc 6"
prim__usleep : Int -> PrimIO Int

safeSleep : Int -> IO ()
safeSleep us = ignore $ primIO (prim__usleep us)

readCommand : String -> IO (Maybe String)
readCommand cmd = do
  Right handle <- popen cmd Read
    | Left _ => pure Nothing
  Right content <- fGetLine handle
    | Left _ => pure Nothing
  ignore $ pclose handle
  pure (Just (trim content))

readCommandNat : String -> IO (Maybe Nat)
readCommandNat cmd = do
  output <- readCommand cmd
  case output of
    Nothing => pure Nothing
    Just text =>
      case parseInteger text of
        Just num => if num > 0 then pure (Just (fromInteger num)) else pure Nothing
        Nothing => pure Nothing

getEnvNat : String -> IO (Maybe Nat)
getEnvNat env = do
  val <- getEnv env
  case val of
    Nothing => pure Nothing
    Just text =>
      case parseInteger text of
        Just num => if num > 0 then pure (Just (fromInteger num)) else pure Nothing
        Nothing => pure Nothing

querySize : IO (Nat, Nat)
querySize = do
  rows <- readCommandNat "tput lines"
  cols <- readCommandNat "tput cols"
  case (rows, cols) of
    (Just r, Just c) => pure (r, c)
    _ => do
      envRows <- getEnvNat "LINES"
      envCols <- getEnvNat "COLUMNS"
      let fallbackRows = fromMaybe 24 envRows
      let fallbackCols = fromMaybe 80 envCols
      pure (fallbackRows, fallbackCols)

export
TerminalBackend IO where
  initBackend = do
    _ <- Penultimate.Input.enableRaw
    putStr Penultimate.Ansi.hideCursor
    putStr Penultimate.Ansi.clearScreen
    putStr (Penultimate.Ansi.cursorTo 1 1)
  shutdownBackend = do
    _ <- Penultimate.Input.disableRaw
    putStr Penultimate.Ansi.resetAttrs
    putStr Penultimate.Ansi.showCursor
  clearScreen = putStr Penultimate.Ansi.clearScreen
  drawChar r c sc = putStr (Penultimate.Ansi.cursorTo (finToNat r + 1) (finToNat c + 1) ++ styleSeq sc.style ++ cast sc.char)
  drawLine r c chars _ = pure () -- Add run-length optimization here later if desired
  drawRect r c rect _ _ = pure ()
  flush = fflush stdout
  readChar = getChar
  pollChar = do
    res <- readCharMaybe
    pure res
  getSize = querySize
  resizePending = Penultimate.Signal.resizePending
  getCapabilities = detectCapabilities
  sleep ms = if ms >= 0 then safeSleep (ms * 1000) else pure ()
