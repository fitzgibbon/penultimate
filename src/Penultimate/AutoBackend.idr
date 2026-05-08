module Penultimate.AutoBackend

import Penultimate.Backend
import Penultimate.Capabilities
import Penultimate.SystemBackend
import Penultimate.BrowserBackend
import System

-- Note: Idris 2 doesn't yet have dynamic target resolution at runtime or clean macros.
-- We expose a stub for compilation here that uses SystemBackend directly for auto,
-- but practically users will import either SystemBackend or BrowserBackend.
