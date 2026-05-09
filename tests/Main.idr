module Main

import Data.Fin
import Data.Vect
import Penultimate.Core
import Penultimate.Headless
import Control.Monad.Identity

assertEq : (Eq a, Show a) => a -> a -> IO ()
assertEq x y = if x == y then putStrLn "Test passed" else putStrLn ("Test failed: " ++ show x ++ " != " ++ show y)

-- A dummy show for testing
Show Color where
  show (Named n) = "Named " ++ show n
  show (RGB r g b) = "RGB " ++ show r ++ " " ++ show g ++ " " ++ show b

Show Attr where
  show (MkAttr b u i) = "MkAttr " ++ show b ++ " " ++ show u ++ " " ++ show i

Show StyledChar where
  show (MkStyledChar c f b a) = "MkStyledChar " ++ show c ++ " " ++ show f ++ " " ++ show b ++ " " ++ show a

testDrawRect : IO ()
testDrawRect = do
  let surface : HeadlessSurface 5 5 = emptyHeadless
  let c = MkStyledChar 'X' (Named 1) (Named 2) defaultAttr
  -- x=1, y=1, w=3, h=3
  let Id (MkHeadless grid) = drawRect surface (FS FZ) (FS FZ) (FS (FS (FS FZ))) (FS (FS (FS FZ))) c

  -- Spot check a few cells
  assertEq (index (FS (FS FZ)) (index (FS (FS FZ)) grid)) c
  assertEq (index FZ (index FZ grid)) defaultStyledChar

main : IO ()
main = do
  putStrLn "Running tests..."
  testDrawRect
