module Penultimate.Canvas

import Data.Fin
import Data.Vect
import Penultimate.Cell

public export
record BorderChars where
  constructor MkBorderChars
  topLeft : Char
  topRight : Char
  bottomLeft : Char
  bottomRight : Char
  horizontal : Char
  vertical : Char

public export
defaultBorder : BorderChars
defaultBorder = MkBorderChars '╭' '╮' '╰' '╯' '─' '│'

public export
record Canvas (width : Nat) (height : Nat) where
  constructor MkCanvas
  rows : Vect height (Vect width Cell)

public export
record AnyCanvas where
  constructor MkAnyCanvas
  width : Nat
  height : Nat
  canvas : Canvas width height

indexedVect : (n : Nat) -> Vect n Nat
indexedVect Z = []
indexedVect (S k) = 0 :: map (\value => value + 1) (indexedVect k)

export
canvasFrom : {width : Nat} -> {height : Nat} -> (Nat -> Nat -> Cell) -> Canvas width height
canvasFrom {height} {width} cellAt =
  let rs = indexedVect height
      cs = indexedVect width
      buildRow : Nat -> Vect width Cell
      buildRow row = map (\col => cellAt row col) cs
   in MkCanvas (map buildRow rs)

export
emptyCanvas : {width : Nat} -> {height : Nat} -> Cell -> Canvas width height
emptyCanvas {height} {width} cell =
  MkCanvas (replicate height (replicate width cell))

export
canvasFromRows : {width : Nat} -> {height : Nat} -> Vect height (Vect width Cell) -> Canvas width height
canvasFromRows rows = MkCanvas rows

natToFinBound : (bound : Nat) -> (value : Nat) -> Maybe (Fin bound)
natToFinBound Z _ = Nothing
natToFinBound (S k) Z = Just FZ
natToFinBound (S k) (S value) = map FS (natToFinBound k value)

zipList : List a -> List b -> List (a, b)
zipList [] _ = []
zipList _ [] = []
zipList (x :: xs) (y :: ys) = (x, y) :: zipList xs ys

subNat : Nat -> Nat -> Nat
subNat Z _ = Z
subNat n Z = n
subNat (S n) (S m) = subNat n m

export
setCell : {width : Nat} -> {height : Nat} -> Nat -> Nat -> Cell -> Canvas width height -> Canvas width height
setCell {width} {height} row col cell (MkCanvas rows) =
  case (natToFinBound height row, natToFinBound width col) of
    (Just rowIndex, Just colIndex) =>
      let updateRow : Vect width Cell -> Vect width Cell
          updateRow rowCells = replaceAt colIndex cell rowCells
          updatedRow = updateRow (index rowIndex rows)
       in MkCanvas (replaceAt rowIndex updatedRow rows)
    _ => MkCanvas rows

export
drawText : {width : Nat} -> {height : Nat} -> Nat -> Nat -> String -> Cell -> Canvas width height -> Canvas width height
drawText row col text cell canvas =
  let chars = unpack text
      apply : (Nat, Char) -> Canvas width height -> Canvas width height
      apply (offset, ch) acc = setCell row (col + offset) (withChar cell ch) acc
   in foldl (flip apply) canvas (zipList (indexed (length chars)) chars)
  where
    indexed : Nat -> List Nat
    indexed Z = []
    indexed (S k) = 0 :: map (\n => n + 1) (indexed k)

export
drawBorder : {width : Nat} -> {height : Nat} -> BorderChars -> Cell -> Canvas width height -> Canvas width height
drawBorder {width} {height} chars cell canvas =
  let topRow = 0
      leftCol = 0
      rightCol = if width == 0 then 0 else subNat width 1
      bottomRow = if height == 0 then 0 else subNat height 1
      c1 = setCell topRow leftCol (withChar cell chars.topLeft) canvas
      c2 = setCell topRow rightCol (withChar cell chars.topRight) c1
      c3 = setCell bottomRow leftCol (withChar cell chars.bottomLeft) c2
      c4 = setCell bottomRow rightCol (withChar cell chars.bottomRight) c3
      drawHoriz : Nat -> Canvas width height -> Canvas width height
      drawHoriz col = setCell topRow col (withChar cell chars.horizontal)
      drawHorizBottom : Nat -> Canvas width height -> Canvas width height
      drawHorizBottom col = setCell bottomRow col (withChar cell chars.horizontal)
      drawVert : Nat -> Canvas width height -> Canvas width height
      drawVert row = setCell row leftCol (withChar cell chars.vertical)
      drawVertRight : Nat -> Canvas width height -> Canvas width height
      drawVertRight row = setCell row rightCol (withChar cell chars.vertical)
      cols = range 1 (if width == 0 then 0 else subNat width 2)
      rows = range 1 (if height == 0 then 0 else subNat height 2)
      c5 = foldl (flip drawHoriz) c4 cols
      c6 = foldl (flip drawHorizBottom) c5 cols
      c7 = foldl (flip drawVert) c6 rows
   in foldl (flip drawVertRight) c7 rows
  where
    range : Nat -> Nat -> List Nat
    range start end = if start > end then [] else start :: range (start + 1) end

export
fillRect : {width : Nat} -> {height : Nat} -> Nat -> Nat -> Nat -> Nat -> Cell -> Canvas width height -> Canvas width height
fillRect startRow startCol h w cell canvas =
  let rows = range startRow (startRow + subNat h 1)
      cols = range startCol (startCol + subNat w 1)
      drawCol : Nat -> Nat -> Canvas width height -> Canvas width height
      drawCol r c acc = setCell r c cell acc
      drawRow : Nat -> Canvas width height -> Canvas width height
      drawRow r acc = foldl (flip (drawCol r)) acc cols
   in foldl (flip drawRow) canvas rows
  where
    range : Nat -> Nat -> List Nat
    range start end = if start > end then [] else start :: range (start + 1) end

export
drawRect : {width : Nat} -> {height : Nat} -> BorderChars -> Nat -> Nat -> Nat -> Nat -> Cell -> Canvas width height -> Canvas width height
drawRect chars startRow startCol h w cell canvas =
  if w == 0 || h == 0 then canvas else
  let topRow = startRow
      leftCol = startCol
      rightCol = startCol + subNat w 1
      bottomRow = startRow + subNat h 1
      c1 = setCell topRow leftCol (withChar cell chars.topLeft) canvas
      c2 = setCell topRow rightCol (withChar cell chars.topRight) c1
      c3 = setCell bottomRow leftCol (withChar cell chars.bottomLeft) c2
      c4 = setCell bottomRow rightCol (withChar cell chars.bottomRight) c3
      drawHoriz : Nat -> Canvas width height -> Canvas width height
      drawHoriz col = setCell topRow col (withChar cell chars.horizontal)
      drawHorizBottom : Nat -> Canvas width height -> Canvas width height
      drawHorizBottom col = setCell bottomRow col (withChar cell chars.horizontal)
      drawVert : Nat -> Canvas width height -> Canvas width height
      drawVert row = setCell row leftCol (withChar cell chars.vertical)
      drawVertRight : Nat -> Canvas width height -> Canvas width height
      drawVertRight row = setCell row rightCol (withChar cell chars.vertical)
      cols = range (startCol + 1) (if w <= 2 then startCol else startCol + subNat w 2)
      rows = range (startRow + 1) (if h <= 2 then startRow else startRow + subNat h 2)
      c5 = foldl (flip drawHoriz) c4 cols
      c6 = foldl (flip drawHorizBottom) c5 cols
      c7 = foldl (flip drawVert) c6 rows
   in foldl (flip drawVertRight) c7 rows
  where
    range : Nat -> Nat -> List Nat
    range start end = if start > end then [] else start :: range (start + 1) end

export
emptyAnyCanvas : Nat -> Nat -> Cell -> AnyCanvas
emptyAnyCanvas height width cell =
  MkAnyCanvas width height (emptyCanvas {width=width} {height=height} cell)

export
anyCanvasFrom : Nat -> Nat -> (Nat -> Nat -> Cell) -> AnyCanvas
anyCanvasFrom height width cellAt =
  MkAnyCanvas width height (canvasFrom {width=width} {height=height} cellAt)

export
setAnyCell : Nat -> Nat -> Cell -> AnyCanvas -> AnyCanvas
setAnyCell row col cell (MkAnyCanvas w h c) =
  MkAnyCanvas w h (setCell row col cell c)

export
drawAnyText : Nat -> Nat -> String -> Cell -> AnyCanvas -> AnyCanvas
drawAnyText row col text cell (MkAnyCanvas w h c) =
  MkAnyCanvas w h (drawText row col text cell c)

export
drawAnyRect : BorderChars -> Nat -> Nat -> Nat -> Nat -> Cell -> AnyCanvas -> AnyCanvas
drawAnyRect chars startRow startCol height width cell (MkAnyCanvas w h c) =
  MkAnyCanvas w h (drawRect chars startRow startCol height width cell c)

export
fillAnyRect : Nat -> Nat -> Nat -> Nat -> Cell -> AnyCanvas -> AnyCanvas
fillAnyRect startRow startCol height width cell (MkAnyCanvas w h c) =
  MkAnyCanvas w h (fillRect startRow startCol height width cell c)
