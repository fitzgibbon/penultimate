module Penultimate.Render

import Data.Fin
import Data.IORef
import Data.List
import Data.Maybe
import Data.So
import Data.Vect
import Penultimate.Attr
import Penultimate.Capabilities
import Penultimate.Cell
import Penultimate.Color
import Penultimate.Canvas
import Penultimate.Signal
import Penultimate.Backend
import Decidable.Equality

public export
record RenderContext (m : Type -> Type) where
  constructor MkRenderContext
  caps : Capabilities
  policy : RenderPolicy
  tier : ColorTier
  sizeRef : IORef (Nat, Nat)
  lastCanvasRef : IORef (Maybe Canvas)

export
resolveCell : ColorTier -> Cell -> Cell
resolveCell tier cell =
  let fg = resolveColor tier cell.fg
      bg = resolveColor tier cell.bg
   in { fg := fg, bg := bg } cell

export
resolveCanvas : ColorTier -> Canvas -> Canvas
resolveCanvas tier (MkCanvas width height rows) =
  let resolveRow : Vect width Cell -> Vect width Cell
      resolveRow rowCells = map (resolveCell tier) rowCells
   in MkCanvas width height (map resolveRow rows)

rowsToList : Vect height (Vect width Cell) -> List (List Cell)
rowsToList rows = toList (map toList rows)

export
initRenderContext : TerminalBackend m => HasIO m => RenderPolicy -> m (RenderContext m)
initRenderContext policy = do
  caps <- getCapabilities
  let tier = resolveTier caps policy
  size <- getSize
  sizeRef <- liftIO (newIORef size)
  lastRef <- liftIO (newIORef Nothing)
  pure (MkRenderContext caps policy tier sizeRef lastRef)

export
getSizeCtx : TerminalBackend m => HasIO m => RenderContext m -> m (Nat, Nat)
getSizeCtx ctx = liftIO (readIORef ctx.sizeRef)

export
refreshSize : TerminalBackend m => HasIO m => RenderContext m -> m ()
refreshSize ctx = do
  size <- getSize
  liftIO (writeIORef ctx.sizeRef size)

refreshSizeIfNeeded : TerminalBackend m => HasIO m => RenderContext m -> m Bool
refreshSizeIfNeeded ctx = do
  pending <- resizePending
  if pending
     then do
       refreshSize ctx
       pure True
     else pure False

styleFromCell : Cell -> RenderStyle
styleFromCell cell = MkRenderStyle cell.fg cell.bg cell.attrs

Changed : Cell -> Maybe Cell -> Type
Changed cell prev =
  case prev of
    Nothing => ()
    Just value => So (not (cell == value))

falseSo : (value : Bool) -> value = False -> So (not value)
falseSo _ prf = rewrite prf in Oh

public export
record DeltaCell where
  constructor MkDeltaCell
  cell : Cell
  prev : Maybe Cell
  0 changed : Changed cell prev

public export
data RenderStep
  = Move Nat Nat
  | Emit (List DeltaCell)

appendAssoc :
  {value : Type} ->
  (left : List value) ->
  (middle : List value) ->
  (right : List value) ->
  left ++ (middle ++ right) = (left ++ middle) ++ right
appendAssoc [] _ _ = Refl
appendAssoc (entry :: rest) middle right = cong (entry ::) (appendAssoc rest middle right)

evalSteps : List RenderStep -> List DeltaCell
evalSteps [] = []
evalSteps (Move _ _ :: steps) = evalSteps steps
evalSteps (Emit deltas :: steps) = deltas ++ evalSteps steps

evalStepsMove : (row : Nat) -> (col : Nat) -> (steps : List RenderStep) ->
  evalSteps (Move row col :: steps) = evalSteps steps
evalStepsMove _ _ _ = Refl

coalesceSteps : List RenderStep -> List RenderStep
coalesceSteps [] = []
coalesceSteps [step] = [step]
coalesceSteps (Move _ _ :: Move row col :: steps) = coalesceSteps (Move row col :: steps)
coalesceSteps (Emit left :: Emit right :: steps) = coalesceSteps (Emit (left ++ right) :: steps)
coalesceSteps (step :: steps) = step :: coalesceSteps steps

coalesceCorrect : (steps : List RenderStep) -> evalSteps (coalesceSteps steps) = evalSteps steps
coalesceCorrect [] = Refl
coalesceCorrect [step] = Refl
coalesceCorrect (Move row col :: steps) =
  case steps of
    Move nextRow nextCol :: rest =>
      let rec = coalesceCorrect (Move nextRow nextCol :: rest)
       in rewrite rec in Refl
    Emit deltas :: rest => coalesceCorrect (Emit deltas :: rest)
    [] => Refl
coalesceCorrect (Emit deltas :: steps) =
  case steps of
    Emit right :: rest =>
      let rec = coalesceCorrect (Emit (deltas ++ right) :: rest)
       in rewrite rec in
          rewrite appendAssoc deltas right (evalSteps rest) in Refl
    Move row col :: rest =>
      let rec = coalesceCorrect (Move row col :: rest)
       in rewrite rec in Refl
    [] => Refl

renderStepsForRow : Nat -> List Cell -> List Cell -> List RenderStep
renderStepsForRow row cells prevCells = go 0 cells prevCells
  where
    collectRun :
      RenderStyle ->
      List Cell ->
      List Cell ->
      (List DeltaCell, List Cell, List Cell, Nat)
    collectRun _ [] prevs = ([], [], prevs, 0)
    collectRun style (cell :: rest) prevs =
      case prevs of
        prev :: prevRest =>
          case decEq (cell == prev) False of
            Yes prf =>
              if styleFromCell cell == style then
                let 0 changed : Changed cell (Just prev) = falseSo (cell == prev) prf
                    delta = MkDeltaCell cell (Just prev) changed
                    (cellsAcc, cellsLeft, prevLeft, len) = collectRun style rest prevRest
                 in (delta :: cellsAcc, cellsLeft, prevLeft, len + 1)
              else ([], cell :: rest, prev :: prevRest, 0)
            No _ => ([], cell :: rest, prev :: prevRest, 0)
        [] =>
          if styleFromCell cell == style then
            let 0 changed : Changed cell Nothing = ()
                delta = MkDeltaCell cell Nothing changed
                (cellsAcc, cellsLeft, prevLeft, len) = collectRun style rest []
             in (delta :: cellsAcc, cellsLeft, prevLeft, len + 1)
          else ([], cell :: rest, [], 0)

    mutual
      go : Nat -> List Cell -> List Cell -> List RenderStep
      go _ [] _ = []
      go col (cell :: rest) (prev :: prevRest) =
        case decEq (cell == prev) False of
          Yes _ => emitRun col (cell :: rest) (prev :: prevRest)
          No _ => go (col + 1) rest prevRest
      go col (cell :: rest) [] = emitRun col (cell :: rest) []

      emitRun : Nat -> List Cell -> List Cell -> List RenderStep
      emitRun col (cell :: rest) prevs =
        let style = styleFromCell cell
            (runCells, cellsLeft, prevLeft, len) = collectRun style (cell :: rest) prevs
         in Move row col :: Emit runCells :: go (col + len) cellsLeft prevLeft
      emitRun _ [] _ = []

renderRow : TerminalBackend m => Nat -> List Cell -> List Cell -> m ()
renderRow row cells prevCells = do
  let 0 _ = coalesceCorrect (renderStepsForRow row cells prevCells)
  go 0 cells prevCells
  where
    cellsToString : List Cell -> String
    cellsToString runCells = pack (map (\cell => cell.ch) runCells)

    collectRun : RenderStyle -> List Cell -> List Cell -> (List Cell, List Cell, List Cell, Nat)
    collectRun _ [] prevs = ([], [], prevs, 0)
    collectRun style (c :: cs) prevs =
      case prevs of
        p :: ps =>
          if c == p then ([], c :: cs, p :: ps, 0)
          else if styleFromCell c == style then
            let (cellsAcc, restCells, restPrev, len) = collectRun style cs ps
             in (c :: cellsAcc, restCells, restPrev, len + 1)
          else ([], c :: cs, p :: ps, 0)
        [] =>
          if styleFromCell c == style then
            let (cellsAcc, restCells, restPrev, len) = collectRun style cs []
             in (c :: cellsAcc, restCells, restPrev, len + 1)
          else ([], c :: cs, [], 0)

    mutual
      go : Nat -> List Cell -> List Cell -> m ()
      go _ [] _ = pure ()
      go col (c :: cs) (p :: ps) =
        if c == p
           then go (col + 1) cs ps
           else emitRun col (c :: cs) (p :: ps)
      go col (c :: cs) [] = emitRun col (c :: cs) []

      emitRun : Nat -> List Cell -> List Cell -> m ()
      emitRun col (c :: cs) prevs = do
        let style = styleFromCell c
        let (runCells, restCells, restPrev, runLen) = collectRun style (c :: cs) prevs
        let text = cellsToString runCells

        drawTextAt (row + 1) (col + 1) style text

        go (col + runLen) restCells restPrev

      emitRun _ [] _ = pure ()

renderRows : TerminalBackend m => Nat -> List (List Cell) -> List (List Cell) -> m ()
renderRows _ [] _ = pure ()
renderRows row (c :: cs) (p :: ps) = do
  renderRow row c p
  renderRows (row + 1) cs ps
renderRows row (c :: cs) [] = do
  renderRow row c []
  renderRows (row + 1) cs []

export
renderCanvas : TerminalBackend m => HasIO m => RenderContext m -> Canvas -> m ()
renderCanvas ctx canvas = do
  resized <- refreshSizeIfNeeded ctx
  lastCanvas <- liftIO (readIORef ctx.lastCanvasRef)
  let resolved = resolveCanvas ctx.tier canvas
  let resolvedRows = rowsToList resolved.rows
  let sizeChanged = case lastCanvas of
        Just prev => prev.width /= resolved.width || prev.height /= resolved.height
        Nothing => False
  if resized || sizeChanged
     then do
       liftIO (writeIORef ctx.lastCanvasRef Nothing)
       clearScreen
     else pure ()
  case lastCanvas of
    Nothing => renderRows 0 resolvedRows []
    Just prev =>
      let prevRows = rowsToList prev.rows
       in if resized || sizeChanged
            then renderRows 0 resolvedRows []
            else renderRows 0 resolvedRows prevRows
  flush
  liftIO (writeIORef ctx.lastCanvasRef (Just resolved))
