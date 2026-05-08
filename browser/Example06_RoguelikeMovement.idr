module Example06_RoguelikeMovement

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

loop : Penultimate IO -> Nat -> Nat -> IO ()
loop term px py = do
  (rows, cols) <- refreshSize term
  let bg = defaultCell
  let playerCell = withChar (withFg bg (Ansi16Color BrightMagenta)) '@'
  let wallCell = withChar (withFg bg (Ansi16Color BrightBlack)) '#'

  let canvas1 = emptyCanvas rows cols bg
  let canvas2 = drawRect defaultBorder 0 0 rows cols wallCell canvas1
  let canvas3 = setCell py px playerCell canvas2
  let canvas4 = drawText (rows `minus` 1) 2 " WASD to move, ESC to exit " bg canvas3

  render term canvas4

  key <- readKeyEvent term
  case key of
    KeyChar 'w' _ => loop term px (if py > 1 then py `minus` 1 else py)
    KeyChar 's' _ => loop term px (if py + 2 <= rows then py + 1 else py)
    KeyChar 'a' _ => loop term (if px > 1 then px `minus` 1 else px) py
    KeyChar 'd' _ => loop term (if px + 2 <= cols then px + 1 else px) py
    KeySpecial EscapeKey _ => pure ()
    _ => loop term px py

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  (rows, cols) <- getSize term
  loop term (cols `div` 2) (rows `div` 2)
  shutdown term
