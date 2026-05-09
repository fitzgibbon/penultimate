module Main

import TestsCore

main : IO ()
main = do
  putStrLn "Running browser tests..."
  testDrawRect
