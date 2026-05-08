module Main

import System

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

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, "1"] => Example01_HelloWorld.run
    [_, "2"] => Example02_Colors.run
    [_, "3"] => Example03_InputEcho.run
    [_, "4"] => Example04_BorderedWindow.run
    [_, "5"] => Example05_Animation.run
    [_, "6"] => Example06_RoguelikeMovement.run
    [_, "7"] => Example07_Mandelbrot.run
    [_, "8"] => Example08_MatrixRain.run
    [_, "9"] => Example09_BouncingBall.run
    [_, "10"] => Example10_DrawingApp.run
    _ => do
      putStrLn "Usage: penultimate-example-runner <1-10>"
      putStrLn "1: Hello World"
      putStrLn "2: Colors (TrueColor Gradient)"
      putStrLn "3: Input Echo"
      putStrLn "4: Bordered Window"
      putStrLn "5: Animation (Non-blocking)"
      putStrLn "6: Roguelike Movement"
      putStrLn "7: Mandelbrot Set"
      putStrLn "8: Matrix Rain"
      putStrLn "9: Bouncing Ball"
      putStrLn "10: Drawing App"
