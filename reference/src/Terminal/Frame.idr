module Terminal.Frame

import Data.Fin
import Data.Vect
import Terminal.Cell

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
record Frame where
  constructor MkFrame
  width : Nat
  height : Nat
  rows : Vect height (Vect width Cell)

indexedVect : (n : Nat) -> Vect n Nat
indexedVect Z = []
indexedVect (S k) = 0 :: map (\value => value + 1) (indexedVect k)

export
frameFrom : Nat -> Nat -> (Nat -> Nat -> Cell) -> Frame
frameFrom height width cellAt =
  let rows = indexedVect height
      cols = indexedVect width
      buildRow : Nat -> Vect width Cell
      buildRow row = map (\col => cellAt row col) cols
   in MkFrame width height (map buildRow rows)

export
emptyFrame : Nat -> Nat -> Cell -> Frame
emptyFrame height width cell =
  MkFrame width height (replicate height (replicate width cell))

export
frameFromRows : {height : Nat} -> {width : Nat} -> Vect height (Vect width Cell) -> Frame
frameFromRows rows = MkFrame width height rows

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
setCell : Nat -> Nat -> Cell -> Frame -> Frame
setCell row col cell (MkFrame width height rows) =
  case (natToFinBound height row, natToFinBound width col) of
    (Just rowIndex, Just colIndex) =>
      let updateRow : Vect width Cell -> Vect width Cell
          updateRow rowCells = replaceAt colIndex cell rowCells
          updatedRow = updateRow (index rowIndex rows)
       in MkFrame width height (replaceAt rowIndex updatedRow rows)
    _ => MkFrame width height rows

export
drawText : Nat -> Nat -> String -> Cell -> Frame -> Frame
drawText row col text cell frame =
  let chars = unpack text
      apply : (Nat, Char) -> Frame -> Frame
      apply (offset, ch) acc = setCell row (col + offset) (withChar cell ch) acc
   in foldl (flip apply) frame (zipList (indexed (length chars)) chars)
  where
    indexed : Nat -> List Nat
    indexed Z = []
    indexed (S k) = 0 :: map (\n => n + 1) (indexed k)

export
drawBorder : BorderChars -> Cell -> Frame -> Frame
drawBorder chars cell frame =
  let (MkFrame width height _) = frame
      topRow = 0
      leftCol = 0
      rightCol = if width == 0 then 0 else subNat width 1
      bottomRow = if height == 0 then 0 else subNat height 1
      frame1 = setCell topRow leftCol (withChar cell chars.topLeft) frame
      frame2 = setCell topRow rightCol (withChar cell chars.topRight) frame1
      frame3 = setCell bottomRow leftCol (withChar cell chars.bottomLeft) frame2
      frame4 = setCell bottomRow rightCol (withChar cell chars.bottomRight) frame3
      drawHoriz : Nat -> Frame -> Frame
      drawHoriz col = setCell topRow col (withChar cell chars.horizontal)
      drawHorizBottom : Nat -> Frame -> Frame
      drawHorizBottom col = setCell bottomRow col (withChar cell chars.horizontal)
      drawVert : Nat -> Frame -> Frame
      drawVert row = setCell row leftCol (withChar cell chars.vertical)
      drawVertRight : Nat -> Frame -> Frame
      drawVertRight row = setCell row rightCol (withChar cell chars.vertical)
      cols = range 1 (if width == 0 then 0 else subNat width 2)
      rows = range 1 (if height == 0 then 0 else subNat height 2)
      frame5 = foldl (flip drawHoriz) frame4 cols
      frame6 = foldl (flip drawHorizBottom) frame5 cols
      frame7 = foldl (flip drawVert) frame6 rows
   in foldl (flip drawVertRight) frame7 rows
  where
    range : Nat -> Nat -> List Nat
    range start end = if start > end then [] else start :: range (start + 1) end
