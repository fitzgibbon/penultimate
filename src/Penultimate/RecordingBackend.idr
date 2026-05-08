module Penultimate.RecordingBackend

import Penultimate.Backend
import Penultimate.Capabilities
import System
import System.File

public export
record RecordingBackendT (m : Type -> Type) a where
  constructor MkRecording
  runRecording : File -> Int -> m a

export
Functor m => Functor (RecordingBackendT m) where
  map f (MkRecording act) = MkRecording (\fH, startT => map f (act fH startT))

export
Applicative m => Applicative (RecordingBackendT m) where
  pure a = MkRecording (\_, _ => pure a)
  (MkRecording f) <*> (MkRecording a) = MkRecording (\fH, startT => f fH startT <*> a fH startT)

export
Monad m => Monad (RecordingBackendT m) where
  (MkRecording a) >>= f = MkRecording (\fH, startT => do
    val <- a fH startT
    let (MkRecording res) = f val
    res fH startT)

export
HasIO m => HasIO (RecordingBackendT m) where
  liftIO action = MkRecording (\_, _ => liftIO action)

escapeStr : String -> String
escapeStr s = concatMap escapeChar (unpack s)
  where
    escapeChar : Char -> String
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = cast c

%foreign "C:clock_gettime_sec, libc 6"
prim__clock_gettime_sec : Int -> PrimIO Int
%foreign "C:clock_gettime_nsec, libc 6"
prim__clock_gettime_nsec : Int -> PrimIO Int

getMonotonicTimeMs : IO Int
getMonotonicTimeMs = do
  sec <- primIO (prim__clock_gettime_sec 1)
  nsec <- primIO (prim__clock_gettime_nsec 1)
  pure ((sec * 1000) + (nsec `div` 1000000))

export
(TerminalBackend m, HasIO m) => TerminalBackend (RecordingBackendT m) where
  writeString s = MkRecording (\fH, startT => do
    now <- liftIO getMonotonicTimeMs
    let elapsedMs = now - startT
    let elapsedSec = cast {to=Double} elapsedMs / 1000.0
    let jsonLine = "[ " ++ show elapsedSec ++ ", \"o\", \"" ++ escapeStr s ++ "\" ]\n"
    ignore $ liftIO (fPutStr fH jsonLine)
    ignore $ liftIO (fflush fH)
    writeString s)
  flush = MkRecording (\_, _ => flush)
  readChar = MkRecording (\_, _ => readChar)
  pollChar = MkRecording (\_, _ => pollChar)
  getSize = MkRecording (\_, _ => getSize)
  enableRaw = MkRecording (\_, _ => enableRaw)
  disableRaw = MkRecording (\_, _ => disableRaw)
  resizePending = MkRecording (\_, _ => resizePending)
  getCapabilities = MkRecording (\_, _ => getCapabilities)
  sleep ms = MkRecording (\_, _ => Penultimate.Backend.sleep ms)

export
withRecording : HasIO m => File -> RecordingBackendT m a -> m a
withRecording fH (MkRecording act) = do
  startT <- liftIO getMonotonicTimeMs
  let header = "{\"version\": 2, \"width\": 80, \"height\": 24, \"timestamp\": " ++ show (startT `div` 1000) ++ ", \"env\": {\"TERM\": \"xterm-256color\"}}\n"
  ignore $ liftIO (fPutStr fH header)
  act fH startT
