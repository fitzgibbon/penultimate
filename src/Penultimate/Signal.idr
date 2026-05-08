module Penultimate.Signal

%foreign "C:collect_signal, libidris2_support, idris_signal.h"
prim__collectSignal : Int -> PrimIO Int

%foreign "C:handle_next_collected_signal, libidris2_support, idris_signal.h"
prim__handleNextCollectedSignal : PrimIO Int

sigWinchCode : Int
sigWinchCode = 28

export
collectWinch : IO Bool
collectWinch = do
  res <- primIO (prim__collectSignal sigWinchCode)
  pure (res /= -1)

export
resizePending : IO Bool
resizePending = loop False
  where
    loop : Bool -> IO Bool
    loop seen = do
      code <- primIO prim__handleNextCollectedSignal
      case code of
        -1 => pure seen
        _ => if code == sigWinchCode then loop True else loop seen
