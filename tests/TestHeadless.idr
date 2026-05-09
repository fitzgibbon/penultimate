module TestHeadless

import Control.Monad.State
import Penultimate.Backend
import Penultimate.MockBackend
import Penultimate.Canvas
import Penultimate.Cell
import Penultimate.Color
import Penultimate.Capabilities
import Penultimate
import System
import Data.String

basicRenderTest : StateT MockState IO ()
basicRenderTest = do
  term <- initPenultimate PreferBest
  let canvas1 = emptyAnyCanvas 5 5 defaultCell
  let bg = defaultCell
  let textCell = withFg bg (Named Red)
  let canvas2 = drawAnyText 1 1 "OK" textCell canvas1
  render term canvas2
  shutdown term

export
run : IO ()
run = do
  putStrLn "Running headless terminal rendering test..."
  res <- execStateT initMockState basicRenderTest
  let out = res.output

  if isInfixOf "<RECT 1,1>" out && isInfixOf "<RECT 1,2>" out
     then do
       putStrLn "Test passed: Mock backend successfully intercepted semantic rendering calls."
     else do
       putStrLn "Test failed: Mock backend trace did not match expected semantic tokens."
       putStrLn ("Actual trace: " ++ out)
       exitFailure
