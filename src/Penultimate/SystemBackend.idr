module Penultimate.SystemBackend

import Penultimate.Backend
import Penultimate.Capabilities
import Penultimate.Input
import Penultimate.Signal
import Penultimate.Render
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
  writeString s = putStr s
  flush = fflush stdout
  readChar = getChar
  pollChar = do
    res <- readCharMaybe
    pure res
  getSize = querySize
  enableRaw = Penultimate.Input.enableRaw
  disableRaw = Penultimate.Input.disableRaw
  resizePending = Penultimate.Signal.resizePending
  getCapabilities = detectCapabilities
  sleep ms = safeSleep (ms * 1000)
