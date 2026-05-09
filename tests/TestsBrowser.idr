module TestsBrowser

import TestsCore

main : IO ()
main = do
  putStrLn "Running browser tests..."
  testDrawRect
