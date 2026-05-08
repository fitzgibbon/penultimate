module Example01_HelloWorld

import Penultimate
import Penultimate.Backend
import Penultimate.BrowserBackend
import Data.Nat
import Penultimate.Canvas
import Data.Nat
import Penultimate.Cell
import Data.Nat
import Penultimate.Color
import Data.Nat
import Penultimate.Capabilities
import Data.Nat

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  (rows, cols) <- getSize term

  let bg = defaultCell
  let textCell = withFg bg (Ansi16Color BrightGreen)

  let canvas1 = emptyCanvas rows cols bg
  let canvas2 = drawText (rows `div` 2) ((cols `minus` 13) `div` 2) "Hello, World!" textCell canvas1
  let canvas3 = drawText (rows `minus` 2) 2 "Press any key to exit..." bg canvas2

  render term canvas3
  _ <- readKeyEvent term

  shutdown term
