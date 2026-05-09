module Example07_Mandelbrot

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

mandelbrot : Double -> Double -> Int
mandelbrot cx cy = go 0 0 0
  where
    maxIter : Int
    maxIter = 30
    go : Double -> Double -> Int -> Int
    go zx zy i =
      if i == maxIter || (zx*zx + zy*zy) > 4.0 then i
      else go (zx*zx - zy*zy + cx) (2.0*zx*zy + cy) (i + 1)

getColor : Int -> Color
getColor iter =
  if iter == 30 then Named Black
  else Ansi256Color (16 + iter * 4)

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  (rows, cols) <- getSize term

  let bg = defaultCell
  let widthF = cast cols
  let heightF = cast rows

  let drawMandel : Nat -> Nat -> AnyCanvas -> AnyCanvas
      drawMandel r c acc =
        let cx = (cast c / widthF) * 3.5 - 2.5
            cy = (cast r / heightF) * 2.0 - 1.0
            iter = mandelbrot cx cy
            cell = withBg bg (getColor iter)
        in setAnyCell r c cell acc

  let canvas1 = anyCanvasFrom rows cols (\r, c =>
        let cx = (cast c / widthF) * 3.5 - 2.5
            cy = (cast r / heightF) * 2.0 - 1.0
            iter = mandelbrot cx cy
        in withBg bg (getColor iter))

  let canvas2 = drawAnyText 0 0 "Mandelbrot Set. Press any key..." bg canvas1

  render term canvas2
  _ <- readKeyEvent term

  shutdown term
