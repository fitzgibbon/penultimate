module Example05_Animation

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

loop : Penultimate IO -> Nat -> IO ()
loop term frameIdx = do
  mKey <- pollEvent term
  case mKey of
    Just (KeySpecial EscapeKey _) => pure ()
    _ => do
      (rows, cols) <- refreshSize term
      let bg = defaultCell

      let pos = frameIdx `mod` (cols `minus` 10)
      let fgColor = Ansi256Color (cast (16 + (frameIdx `mod` 200)))
      let boxCell = withBg bg fgColor

      let canvas1 = emptyCanvas rows cols bg
      let canvas2 = fillRect 5 pos 5 10 boxCell canvas1
      let canvas3 = drawText 2 2 "Non-blocking Animation (Press ESC to exit)" bg canvas2
      let canvas4 = drawText 12 2 ("Frame: " ++ show frameIdx) bg canvas3

      render term canvas4
      sleep 50 -- 50ms = 20fps
      loop term (frameIdx + 1)

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  loop term 0
  shutdown term
