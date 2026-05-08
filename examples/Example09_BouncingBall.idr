module Example09_BouncingBall

import Penultimate
import Penultimate.Canvas
import Penultimate.Cell
import Penultimate.Color
import Penultimate.Capabilities
import System
import Data.Nat

record Ball where
  constructor MkBall
  x : Double
  y : Double
  dx : Double
  dy : Double

loop : Penultimate -> Ball -> IO ()
loop term ball = do
  mKey <- pollEvent term
  case mKey of
    Just (KeySpecial EscapeKey _) => pure ()
    _ => do
      (rows, cols) <- refreshSize term
      let bg = defaultCell

      let ballCell = withChar (withFg bg (Ansi16Color BrightCyan)) 'O'

      let canvas1 = emptyCanvas rows cols bg
      let canvas2 = drawRect defaultBorder 0 0 rows cols (withFg bg (Ansi16Color BrightBlue)) canvas1
      let canvas3 = setCell (cast ball.y) (cast ball.x) ballCell canvas2
      let canvas4 = drawText 0 2 " Bouncing Ball (Press ESC) " bg canvas3

      render term canvas4
      usleep 30000

      let nx = ball.x + ball.dx
      let ny = ball.y + ball.dy
      let maxX = cast (cols `minus` 2)
      let maxY = cast (rows `minus` 2)

      let ndx = if nx <= 1.0 || nx >= maxX then -ball.dx else ball.dx
      let ndy = if ny <= 1.0 || ny >= maxY then -ball.dy else ball.dy
      let fx = if nx <= 1.0 then 1.0 else if nx >= maxX then maxX else nx
      let fy = if ny <= 1.0 then 1.0 else if ny >= maxY then maxY else ny

      loop term (MkBall fx fy ndx ndy)

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  loop term (MkBall 10.0 5.0 0.8 0.4)
  shutdown term
