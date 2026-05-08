module Main

import Example01_HelloWorld
import Example02_Colors
import Example03_InputEcho
import Example04_BorderedWindow
import Example05_Animation
import Example06_RoguelikeMovement
import Example07_Mandelbrot
import Example08_MatrixRain
import Example09_BouncingBall
import Example10_DrawingApp

export
runExampleBrowser : Int -> IO ()
runExampleBrowser n = case n of
  1 => Example01_HelloWorld.run
  2 => Example02_Colors.run
  3 => Example03_InputEcho.run
  4 => Example04_BorderedWindow.run
  5 => Example05_Animation.run
  6 => Example06_RoguelikeMovement.run
  7 => Example07_Mandelbrot.run
  8 => Example08_MatrixRain.run
  9 => Example09_BouncingBall.run
  10 => Example10_DrawingApp.run
  _ => pure ()

main : IO ()
main = pure ()
