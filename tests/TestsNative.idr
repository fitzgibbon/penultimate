module TestsNative

import TestsCore

main : IO ()
main = do
  putStrLn "Running native tests..."
  testDrawRect
