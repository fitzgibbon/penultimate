module Penultimate.MockBackend

import Control.Monad.State
import Penultimate.Backend
import Penultimate.Capabilities

public export
record MockState where
  constructor MkMockState
  output : String
  inputQueue : List Char

export
initMockState : MockState
initMockState = MkMockState "" []

export
TerminalBackend (StateT MockState IO) where
  writeString s = modify (\st => { output := st.output ++ s } st)
  flush = pure ()
  readChar = do
    st <- get
    case st.inputQueue of
      [] => pure '\0'
      (c :: cs) => do
        put ({ inputQueue := cs } st)
        pure c
  pollChar = do
    st <- get
    case st.inputQueue of
      [] => pure Nothing
      (c :: cs) => do
        put ({ inputQueue := cs } st)
        pure (Just c)
  getSize = pure (24, 80)
  enableRaw = pure True
  disableRaw = pure True
  resizePending = pure False
  getCapabilities = pure (MkCapabilities True True True)
  sleep _ = pure ()
