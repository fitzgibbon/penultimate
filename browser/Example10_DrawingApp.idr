module Example10_DrawingApp

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

loop : Penultimate IO -> Canvas -> Nat -> Nat -> Color -> IO ()
loop term canvas px py currentColor = do
  (rows, cols) <- refreshSize term
  let bg = defaultCell

  -- Overlay cursor
  let cursorCell = withBg bg currentColor
  let displayCanvas = setCell py px cursorCell canvas
  let uiCanvas = drawText 0 0 "Arrows: Move | Space: Draw | C: Change Color | ESC: Exit" bg displayCanvas

  render term uiCanvas

  key <- readKeyEvent term
  case key of
    KeySpecial ArrowUp _ => loop term canvas px (if py > 0 then py `minus` 1 else 0) currentColor
    KeySpecial ArrowDown _ => loop term canvas px (if py + 1 < rows then py + 1 else py) currentColor
    KeySpecial ArrowLeft _ => loop term canvas (if px > 0 then px `minus` 1 else 0) py currentColor
    KeySpecial ArrowRight _ => loop term canvas (if px + 1 < cols then px + 1 else px) py currentColor
    KeyChar ' ' _ =>
      let newCanvas = setCell py px (withBg bg currentColor) canvas
      in loop term newCanvas px py currentColor
    KeyChar 'c' _ =>
      let nextColor = case currentColor of
            Named Red => Named Green
            Named Green => Named Blue
            Named Blue => Named Yellow
            Named Yellow => Named BrightMagenta
            _ => Named Red
      in loop term canvas px py nextColor
    KeySpecial EscapeKey _ => pure ()
    _ => loop term canvas px py currentColor

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  (rows, cols) <- getSize term
  loop term (emptyCanvas rows cols defaultCell) (cols `div` 2) (rows `div` 2) (Named Red)
  shutdown term
