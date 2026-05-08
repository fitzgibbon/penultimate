module Example03_InputEcho

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

formatKey : Key -> String
formatKey (KeyChar c mods) = "Char: " ++ cast c ++ (if mods.shift then " (Shift)" else "")
formatKey (KeySpecial ArrowUp _) = "Up Arrow"
formatKey (KeySpecial ArrowDown _) = "Down Arrow"
formatKey (KeySpecial ArrowLeft _) = "Left Arrow"
formatKey (KeySpecial ArrowRight _) = "Right Arrow"
formatKey (KeySpecial EscapeKey _) = "Escape"
formatKey _ = "Other Key"

loop : Penultimate IO -> Canvas -> String -> IO ()
loop term bgCanvas lastKey = do
  (rows, cols) <- refreshSize term
  let bg = defaultCell
  let textCell = withFg bg (Ansi16Color BrightYellow)

  let canvas1 = drawText 2 2 "Input Echo Example" bg bgCanvas
  let canvas2 = drawText 4 2 ("Last key pressed: " ++ lastKey) textCell canvas1
  let canvas3 = drawText (rows `minus` 2) 2 "Press ESC to exit..." bg canvas2

  render term canvas3

  key <- readKeyEvent term
  case key of
    KeySpecial EscapeKey _ => pure ()
    _ => loop term bgCanvas (formatKey key)

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  (rows, cols) <- getSize term
  loop term (emptyCanvas rows cols defaultCell) "None"
  shutdown term
