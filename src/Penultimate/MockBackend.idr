module Penultimate.MockBackend

import Data.Fin
import Control.Monad.State
import Penultimate.Backend
import Penultimate.Capabilities
import Data.Vect

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
  initBackend = pure ()
  shutdownBackend = pure ()
  clearScreen = modify (\st => { output := st.output ++ "<CLEAR>" } st)
  drawRect r c rect _ _ = modify (\st => { output := st.output ++ "<RECT " ++ show (finToNat r) ++ "," ++ show (finToNat c) ++ ">" } st)
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
  resizePending = pure False
  getCapabilities = pure (MkCapabilities True True True)
  sleep _ = pure ()
