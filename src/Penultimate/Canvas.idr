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
record Canvas where
  constructor MkCanvas
  width : Nat
  height : Nat
  rows : Vect height (Vect width Cell)

indexedVect : (n : Nat) -> Vect n Nat
indexedVect Z = []
indexedVect (S k) = 0 :: map (\value => value + 1) (indexedVect k)

export
canvasFrom : Nat -> Nat -> (Nat -> Nat -> Cell) -> Canvas
canvasFrom height width cellAt =
  let rows = indexedVect height
      cols = indexedVect width
      buildRow : Nat -> Vect width Cell
      buildRow row = map (\col => cellAt row col) cols
   in MkCanvas width height (map buildRow rows)

export
emptyCanvas : Nat -> Nat -> Cell -> Canvas
emptyCanvas height width cell =
  MkCanvas width height (replicate height (replicate width cell))

export
canvasFromRows : {height : Nat} -> {width : Nat} -> Vect height (Vect width Cell) -> Canvas
canvasFromRows rows = MkCanvas width height rows

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
setCell : Nat -> Nat -> Cell -> Canvas -> Canvas
setCell row col cell (MkCanvas width height rows) =
  case (natToFinBound height row, natToFinBound width col) of
    (Just rowIndex, Just colIndex) =>
      let updateRow : Vect width Cell -> Vect width Cell
          updateRow rowCells = replaceAt colIndex cell rowCells
          updatedRow = updateRow (index rowIndex rows)
       in MkCanvas width height (replaceAt rowIndex updatedRow rows)
    _ => MkCanvas width height rows

export
drawText : Nat -> Nat -> String -> Cell -> Canvas -> Canvas
drawText row col text cell canvas =
  let chars = unpack text
      apply : (Nat, Char) -> Canvas -> Canvas
      apply (offset, ch) acc = setCell row (col + offset) (withChar cell ch) acc
   in foldl (flip apply) canvas (zipList (indexed (length chars)) chars)
  where
    indexed : Nat -> List Nat
    indexed Z = []
    indexed (S k) = 0 :: map (\n => n + 1) (indexed k)

export
drawBorder : BorderChars -> Cell -> Canvas -> Canvas
drawBorder chars cell canvas =
  let (MkCanvas width height _) = canvas
      topRow = 0
      leftCol = 0
      rightCol = if width == 0 then 0 else subNat width 1
      bottomRow = if height == 0 then 0 else subNat height 1
      canvas1 = setCell topRow leftCol (withChar cell chars.topLeft) canvas
      canvas2 = setCell topRow rightCol (withChar cell chars.topRight) canvas1
      canvas3 = setCell bottomRow leftCol (withChar cell chars.bottomLeft) canvas2
      canvas4 = setCell bottomRow rightCol (withChar cell chars.bottomRight) canvas3
      drawHoriz : Nat -> Canvas -> Canvas
      drawHoriz col = setCell topRow col (withChar cell chars.horizontal)
      drawHorizBottom : Nat -> Canvas -> Canvas
      drawHorizBottom col = setCell bottomRow col (withChar cell chars.horizontal)
      drawVert : Nat -> Canvas -> Canvas
      drawVert row = setCell row leftCol (withChar cell chars.vertical)
      drawVertRight : Nat -> Canvas -> Canvas
      drawVertRight row = setCell row rightCol (withChar cell chars.vertical)
      cols = range 1 (if width == 0 then 0 else subNat width 2)
      rows = range 1 (if height == 0 then 0 else subNat height 2)
      canvas5 = foldl (flip drawHoriz) canvas4 cols
      canvas6 = foldl (flip drawHorizBottom) canvas5 cols
      canvas7 = foldl (flip drawVert) canvas6 rows
   in foldl (flip drawVertRight) canvas7 rows
  where
    range : Nat -> Nat -> List Nat
    range start end = if start > end then [] else start :: range (start + 1) end

export
fillRect : Nat -> Nat -> Nat -> Nat -> Cell -> Canvas -> Canvas
fillRect startRow startCol h w cell canvas =
  let rows = range startRow (startRow + subNat h 1)
      cols = range startCol (startCol + subNat w 1)
      drawCol : Nat -> Nat -> Canvas -> Canvas
      drawCol r c acc = setCell r c cell acc
      drawRow : Nat -> Canvas -> Canvas
      drawRow r acc = foldl (flip (drawCol r)) acc cols
   in foldl (flip drawRow) canvas rows
  where
    range : Nat -> Nat -> List Nat
    range start end = if start > end then [] else start :: range (start + 1) end

export
drawRect : BorderChars -> Nat -> Nat -> Nat -> Nat -> Cell -> Canvas -> Canvas
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
      drawHoriz : Nat -> Canvas -> Canvas
      drawHoriz col = setCell topRow col (withChar cell chars.horizontal)
      drawHorizBottom : Nat -> Canvas -> Canvas
      drawHorizBottom col = setCell bottomRow col (withChar cell chars.horizontal)
      drawVert : Nat -> Canvas -> Canvas
      drawVert row = setCell row leftCol (withChar cell chars.vertical)
      drawVertRight : Nat -> Canvas -> Canvas
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
