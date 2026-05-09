module Example02_Colors

import Penultimate
import Penultimate.Backend
import Penultimate.SystemBackend
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
  term <- initPenultimate ForceTrueColor
  (rows, cols) <- getSize term

  let bg = defaultCell

  let drawGradient : Nat -> AnyCanvas -> AnyCanvas
      drawGradient Z c = c
      drawGradient (S k) c =
        let r = cast ((k * 255) `div` (cols + 1))
            g = 128
            b = cast (((cols `minus` k) * 255) `div` (cols + 1))
            color = RGBColor (mkRGB r g b)
            cell = withBg bg color
        in drawGradient k (fillAnyRect 2 k (rows `minus` 4) 1 cell c)

  let canvas1 = emptyAnyCanvas rows cols bg
  let canvas2 = drawAnyText 0 2 "TrueColor Gradient Example" bg canvas1
  let canvas3 = drawGradient cols canvas2
  let canvas4 = drawAnyText (rows `minus` 1) 2 "Press any key to exit..." bg canvas3

  render term canvas4
  _ <- readKeyEvent term

  shutdown term
