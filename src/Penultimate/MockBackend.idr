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
  drawChar r c sc = modify (\st => { output := st.output ++ "<CHAR " ++ show (finToNat r) ++ "," ++ show (finToNat c) ++ " '" ++ cast sc.char ++ "'>" } st)
  drawLine r c chars _ = pure ()
  drawRect r c rect _ _ = pure ()
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
