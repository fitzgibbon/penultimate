module Penultimate.Render

import Data.IORef
import Data.List
import Data.Maybe
import Data.So
import Data.String
import Data.Vect
import Decidable.Equality
import System
import System.File.Mode
import System.File.Process
import System.File.ReadWrite
import System.File.Virtual
import System.File.Types
import PrimIO
import Penultimate.Ansi
import Penultimate.Attr
import Penultimate.Capabilities
import Penultimate.Cell
import Penultimate.Color
import Penultimate.Canvas
import Penultimate.Signal

public export
record RenderContext where
  constructor MkRenderContext
  capabilities : Capabilities
  policy : RenderPolicy
  tier : ColorTier
  sizeRef : IORef (Nat, Nat)
  lastCanvasRef : IORef (Maybe Canvas)

%foreign "C:setvbuf,libc 6"
prim__setvbuf : FilePtr -> AnyPtr -> Int -> Int -> PrimIO Int

bufferModeNone : Int
bufferModeNone = 2

setStdoutUnbuffered : IO ()
setStdoutUnbuffered =
  case stdout of
    FHandle handle => do
      _ <- primIO (prim__setvbuf handle prim__getNullAnyPtr bufferModeNone 0)
      pure ()

blendRGB : RGB -> RGB -> Int -> RGB
blendRGB (MkRGB fr fg fb) (MkRGB br bg bb) alpha =
  let t = clampAlpha alpha
      fgWeight = 255 - t
      bgWeight = t
      mix : Int -> Int -> Int
      mix f b = div ((f * fgWeight) + (b * bgWeight)) 255
   in mkRGB (mix (channelValue fr) (channelValue br))
            (mix (channelValue fg) (channelValue bg))
            (mix (channelValue fb) (channelValue bb))
  where
    clampAlpha : Int -> Int
    clampAlpha value =
      if value < 0 then 0 else if value > 255 then 255 else value

resolveCell : ColorTier -> Cell -> Cell
resolveCell tier cell =
  let fgRGB = colorToRGB cell.fg
      bgRGB = colorToRGB cell.bg
      blended = blendRGB fgRGB bgRGB cell.alpha
      fgResolved = resolveColor tier (RGBColor blended)
      bgResolved = resolveColor tier cell.bg
   in { fg := fgResolved, bg := bgResolved } cell

resolveCanvas : ColorTier -> Canvas -> Canvas
resolveCanvas tier (MkCanvas width height rows) =
  MkCanvas width height (map (map (resolveCell tier)) rows)

rowsToList : Vect rowCount (Vect colCount Cell) -> List (List Cell)
rowsToList rows = map toList (toList rows)

getEnvNat : String -> IO (Maybe Nat)
getEnvNat name = do
  raw <- getEnv name
  case raw of
    Nothing => pure Nothing
    Just value =>
      case parseInteger value of
        Just num => if num > 0 then pure (Just (fromInteger num)) else pure Nothing
        Nothing => pure Nothing

readCommand : String -> IO (Maybe String)
readCommand cmd = do
  res <- popen cmd Read
  case res of
    Left _ => pure Nothing
    Right handle => do
      output <- fRead handle
      _ <- pclose handle
      case output of
        Left _ => pure Nothing
        Right text => pure (Just text)

readCommandNat : String -> IO (Maybe Nat)
readCommandNat cmd = do
  output <- readCommand cmd
  case output of
    Nothing => pure Nothing
    Just text =>
      case parseInteger text of
        Just num => if num > 0 then pure (Just (fromInteger num)) else pure Nothing
        Nothing => pure Nothing

querySize : IO (Nat, Nat)
querySize = do
  rows <- readCommandNat "tput lines"
  cols <- readCommandNat "tput cols"
  case (rows, cols) of
    (Just r, Just c) => pure (r, c)
    _ => do
      envRows <- getEnvNat "LINES"
      envCols <- getEnvNat "COLUMNS"
      let fallbackRows = fromMaybe 24 envRows
      let fallbackCols = fromMaybe 80 envCols
      pure (fallbackRows, fallbackCols)

export
initRenderContext : RenderPolicy -> IO RenderContext
initRenderContext policy = do
  setStdoutUnbuffered
  caps <- detectCapabilities
  let tier = resolveTier caps policy
  size <- querySize
  sizeRef <- newIORef size
  lastRef <- newIORef Nothing
  pure (MkRenderContext caps policy tier sizeRef lastRef)

export
getSize : RenderContext -> IO (Nat, Nat)
getSize ctx = readIORef ctx.sizeRef

export
refreshSize : RenderContext -> IO ()
refreshSize ctx = do
  size <- querySize
  writeIORef ctx.sizeRef size

refreshSizeIfNeeded : RenderContext -> IO Bool
refreshSizeIfNeeded ctx = do
  pending <- resizePending
  if pending
     then do
       refreshSize ctx
       pure True
     else pure False

record RenderStyle where
  constructor MkRenderStyle
  fgStyle : Color
  bgStyle : Color
  attrsStyle : Attrs

export
Eq RenderStyle where
  (MkRenderStyle fg1 bg1 attrs1) == (MkRenderStyle fg2 bg2 attrs2) =
    fg1 == fg2 && bg1 == bg2 && attrs1 == attrs2

styleFromCell : Cell -> RenderStyle
styleFromCell cell = MkRenderStyle cell.fg cell.bg cell.attrs

styleSeq : RenderStyle -> String
styleSeq style =
  let attrSeq = concat (map setAttr style.attrsStyle)
      fgSeq = case style.fgStyle of
        RGBColor rgb => setFgTrueColor rgb
        Ansi256Color idx => setFgAnsi256 idx
        Ansi16Color name => setFgAnsi16 name
        Named name => setFgAnsi16 name
        Custom user =>
          case user.ansi16 of
            Just name => setFgAnsi16 name
            Nothing => setFgAnsi16 White
      bgSeq = case style.bgStyle of
        RGBColor rgb => setBgTrueColor rgb
        Ansi256Color idx => setBgAnsi256 idx
        Ansi16Color name => setBgAnsi16 name
        Named name => setBgAnsi16 name
        Custom user =>
          case user.ansi16 of
            Just name => setBgAnsi16 name
            Nothing => setBgAnsi16 Black
   in resetAttrs ++ attrSeq ++ fgSeq ++ bgSeq

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

renderRow : Nat -> List Cell -> List Cell -> String
renderRow row cells prevCells =
  let 0 _ = coalesceCorrect (renderStepsForRow row cells prevCells) in
  go 0 cells prevCells Nothing
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
      go : Nat -> List Cell -> List Cell -> Maybe RenderStyle -> String
      go _ [] _ _ = ""
      go col (c :: cs) (p :: ps) currentStyle =
        if c == p
           then go (col + 1) cs ps currentStyle
           else emitRun col (c :: cs) (p :: ps) currentStyle
      go col (c :: cs) [] currentStyle = emitRun col (c :: cs) [] currentStyle

      emitRun : Nat -> List Cell -> List Cell -> Maybe RenderStyle -> String
      emitRun col (c :: cs) prevs currentStyle =
        let style = styleFromCell c
            (runCells, restCells, restPrev, runLen) = collectRun style (c :: cs) prevs
            text = cellsToString runCells
            stylePrefix =
              case currentStyle of
                Just current => if current == style then "" else styleSeq style
                Nothing => styleSeq style
         in cursorTo (row + 1) (col + 1) ++ stylePrefix ++ text ++
            go (col + runLen) restCells restPrev (Just style)
      emitRun _ [] _ currentStyle = go 0 [] [] currentStyle

renderRows : Nat -> List (List Cell) -> List (List Cell) -> String
renderRows _ [] _ = ""
renderRows row (c :: cs) (p :: ps) = renderRow row c p ++ renderRows (row + 1) cs ps
renderRows row (c :: cs) [] = renderRow row c [] ++ renderRows (row + 1) cs []

export
renderCanvas : RenderContext -> Canvas -> IO ()
renderCanvas ctx canvas = do
  resized <- refreshSizeIfNeeded ctx
  lastCanvas <- readIORef ctx.lastCanvasRef
  let resolved = resolveCanvas ctx.tier canvas
  let resolvedRows = rowsToList resolved.rows
  let sizeChanged = case lastCanvas of
        Just prev => prev.width /= resolved.width || prev.height /= resolved.height
        Nothing => False
  if resized || sizeChanged
     then do
       writeIORef ctx.lastCanvasRef Nothing
       putStr clearScreen
       putStr (cursorTo 1 1)
     else pure ()
  let output = case lastCanvas of
        Nothing => renderRows 0 resolvedRows []
        Just prev =>
          let prevRows = rowsToList prev.rows
           in if resized || sizeChanged
                then renderRows 0 resolvedRows []
                else renderRows 0 resolvedRows prevRows
  putStr output
  fflush stdout
  writeIORef ctx.lastCanvasRef (Just resolved)
