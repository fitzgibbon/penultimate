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
  lastCanvasRef : IORef (Maybe AnyCanvas)

export
resolveCell : ColorTier -> Cell -> Cell
resolveCell tier cell =
  let fg = resolveColor tier cell.fg
      bg = resolveColor tier cell.bg
   in { fg := fg, bg := bg } cell

export
resolveCanvas : ColorTier -> AnyCanvas -> AnyCanvas
resolveCanvas tier (MkAnyCanvas width height (MkCanvas rows)) =
  let resolveRow : Vect width Cell -> Vect width Cell
      resolveRow rowCells = map (resolveCell tier) rowCells
   in MkAnyCanvas width height (MkCanvas (map resolveRow rows))

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

styledFromCell : Cell -> StyledChar
styledFromCell cell = MkStyledChar cell.ch (styleFromCell cell)

export
renderCanvas : TerminalBackend m => HasIO m => RenderContext m -> AnyCanvas -> m ()
renderCanvas ctx canvas = do
  resized <- refreshSizeIfNeeded ctx
  lastCanvas <- liftIO (readIORef ctx.lastCanvasRef)

  let resolved = resolveCanvas ctx.tier canvas
  let sizeChanged = case lastCanvas of
        Just prev => prev.width /= resolved.width || prev.height /= resolved.height
        Nothing => False

  if resized || sizeChanged
     then do
       liftIO (writeIORef ctx.lastCanvasRef Nothing)
       clearScreen
     else pure ()

  let (MkAnyCanvas currW currH (MkCanvas currRs)) = resolved

  let mprev : Maybe AnyCanvas = case lastCanvas of
        Nothing => Nothing
        Just p => if currW == p.width && currH == p.height then Just p else Nothing

  let (MkAnyCanvas w h (MkCanvas rows)) = resolved
  let prevRows = case mprev of
        Nothing => replicate h (replicate w defaultCell)
        Just (MkAnyCanvas pw ph (MkCanvas prows)) => believe_me prows

  let goCol : Fin h -> Nat -> m ()
      goCol finR c = case natToFin c w of
        Nothing => pure ()
        Just finC => do
          let cellCurr = index finC (index finR rows)
          let isSame = cellCurr == index finC (index finR prevRows)
          let diff : Bool = case mprev of
                Nothing => True
                Just _ => not isSame
          if diff
             then do
               drawChar finR finC (styledFromCell cellCurr)
               goCol finR (c + 1)
             else do
               goCol finR (c + 1)

  let goRow : Nat -> m ()
      goRow r = case natToFin r h of
        Nothing => pure ()
        Just finR => do
          goCol finR 0
          goRow (r + 1)

  goRow 0

  flush
  liftIO (writeIORef ctx.lastCanvasRef (Just resolved))
