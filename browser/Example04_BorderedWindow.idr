module Example04_BorderedWindow

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
  let winBg = withBg bg (Ansi16Color Blue)
  let winCell = withFg winBg (Ansi16Color BrightWhite)

  let canvas1 = emptyCanvas rows cols bg
  let winWidth = 40
  let winHeight = 10
  let startRow = (rows `minus` winHeight) `div` 2
  let startCol = (cols `minus` winWidth) `div` 2

  let canvas2 = fillRect startRow startCol winHeight winWidth winBg canvas1
  let canvas3 = drawRect defaultBorder startRow startCol winHeight winWidth winCell canvas2
  let canvas4 = drawText (startRow + 2) (startCol + 4) "TUI Window System" winCell canvas3
  let canvas5 = drawText (startRow + 4) (startCol + 4) "With overlapping elements" winCell canvas4
  let canvas6 = drawText (rows `minus` 2) 2 "Press any key to exit..." bg canvas5

  render term canvas6
  _ <- readKeyEvent term

  shutdown term
