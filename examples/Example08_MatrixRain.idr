module Example08_MatrixRain

import Penultimate
import Penultimate.Canvas
import Penultimate.Cell
import Penultimate.Color
import Penultimate.Capabilities
import System
import Data.Vect
import Data.Nat

loop : Penultimate -> Vect 80 Nat -> Nat -> IO ()
loop term drops frameIdx = do
  mKey <- pollEvent term
  case mKey of
    Just (KeySpecial EscapeKey _) => pure ()
    _ => do
      (rows, cols) <- refreshSize term
      let bg = defaultCell

      let mkCell : Nat -> Nat -> Cell
          mkCell r c =
            if c >= 80 then bg else
            let headPos = index (restrict 79 (cast c)) drops
            in if r == headPos then withFg bg (Ansi16Color BrightWhite)
               else if r < headPos && (headPos `minus` r) <= 9 then withFg bg (Ansi16Color BrightGreen)
               else if r < headPos && (headPos `minus` r) <= 19 then withFg bg (Ansi16Color Green)
               else bg

      let canvas1 = canvasFrom rows cols (\r, c => withChar (mkCell r c) (if mkCell r c == bg then ' ' else '|'))
      let canvas2 = drawText 0 0 "Matrix Rain (Press ESC to exit)" bg canvas1

      render term canvas2
      usleep 50000

      -- simple drop update logic
      let newDrops = map (\y => if y > rows + 20 then (the Nat 0) else y + 1) drops
      -- add some randomness manually
      let rIdx = frameIdx `mod` 80
      let updatedDrops = replaceAt (restrict 79 (cast rIdx)) (if frameIdx `mod` 3 == 0 then (the Nat 0) else index (restrict 79 (cast rIdx)) newDrops) newDrops

      loop term updatedDrops (frameIdx + 1)

export
run : IO ()
run = do
  term <- initPenultimate PreferBest
  loop term (replicate 80 0) 0
  shutdown term
