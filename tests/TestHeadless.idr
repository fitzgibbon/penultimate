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

basicRenderTest : StateT MockState IO ()
basicRenderTest = do
  term <- initPenultimate PreferBest
  let canvas1 = emptyCanvas 5 5 defaultCell
  let bg = defaultCell
  let textCell = withFg bg (Named Red)
  let canvas2 = drawText 1 1 "OK" textCell canvas1
  render term canvas2
  shutdown term

export
run : IO ()
run = do
  putStrLn "Running headless terminal rendering test..."
  res <- execStateT initMockState basicRenderTest
  let out = res.output
  if length out > 0
     then do
       putStrLn "Test passed: Mock backend successfully intercepted rendered ANSI buffers."
       putStrLn ("Captured " ++ show (length out) ++ " characters of ANSI output.")
     else do
       putStrLn "Test failed: Mock backend captured no output."
       exitFailure
