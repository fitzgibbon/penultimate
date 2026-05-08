module Penultimate.Backend

import Data.List
import Penultimate.Capabilities
import Penultimate.Input

public export
interface Monad m => TerminalBackend m where
  writeString : String -> m ()
  flush : m ()
  readChar : m Char
  pollChar : m (Maybe Char)
  getSize : m (Nat, Nat)
  enableRaw : m Bool
  disableRaw : m Bool
  resizePending : m Bool
  getCapabilities : m Capabilities
  sleep : Int -> m ()
