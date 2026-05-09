module Console

import Data.Fin
import Penultimate.Core
import Penultimate.TTY

main : IO ()
main = do
  surface <- initTTY {w=10} {h=10}

  let blueBg = MkStyledChar ' ' (Named 7) (Named 4) defaultAttr
  surface2 <- drawRect surface (FS (FS FZ)) (FS (FS FZ)) (FS (FS (FS (FS (FS FZ))))) (FS (FS (FS (FS (FS FZ))))) blueBg

  let redFg = MkStyledChar 'X' (Named 1) (Named 4) (MkAttr True False False)
  surface3 <- drawRect surface2 (FS (FS (FS (FS FZ)))) (FS (FS (FS (FS FZ)))) (FS FZ) (FS FZ) redFg

  putStrLn "\nConsole example rendered!"
