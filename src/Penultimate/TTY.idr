module Penultimate.TTY

import Data.Fin
import Data.Nat
import Data.Vect
import Penultimate.Core
import System
import System.Info
import Data.String

public export
record TTYSurface (w : Nat) (h : Nat) where
  constructor MkTTY
  grid : Vect h (Vect w StyledChar)
  trueColor : Bool

emptyTTY : {w : Nat} -> {h : Nat} -> Bool -> TTYSurface w h
emptyTTY tc = MkTTY
  (replicate h (replicate w defaultStyledChar))
  tc

public export
initTTY : {w : Nat} -> {h : Nat} -> IO (TTYSurface w h)
initTTY = do
  envColorTerm <- getEnv "COLORTERM"
  let isTrueColor = case envColorTerm of
                      Just val => val == "truecolor" || val == "24bit"
                      Nothing => False
  pure (emptyTTY isTrueColor)

ansifyColorBg : Bool -> Color -> String
ansifyColorBg _ (Named n) = "\x1b[" ++ show (n + 40) ++ "m"
ansifyColorBg True (RGB r g b) = "\x1b[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
ansifyColorBg False (RGB r g b) = "\x1b[40m"

ansifyColorFg : Bool -> Color -> String
ansifyColorFg _ (Named n) = "\x1b[" ++ show (n + 30) ++ "m"
ansifyColorFg True (RGB r g b) = "\x1b[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
ansifyColorFg False (RGB r g b) = "\x1b[37m"

ansifyAttr : Attr -> String
ansifyAttr (MkAttr b u i) =
  let s_b = if b then "\x1b[1m" else "\x1b[22m"
      s_u = if u then "\x1b[4m" else "\x1b[24m"
      s_i = if i then "\x1b[3m" else "\x1b[23m"
  in s_b ++ s_u ++ s_i

moveCursor : Nat -> Nat -> String
moveCursor r c = "\x1b[" ++ show (r + 1) ++ ";" ++ show (c + 1) ++ "H"

public export
Surface TTYSurface IO where
  drawRect (MkTTY grid tc) x y rect_w rect_h c = do
    let x_nat = finToNat x
    let y_nat = finToNat y
    let rect_w_nat = finToNat rect_w
    let rect_h_nat = finToNat rect_h

    let indicesToUpdateJ = [ j | j <- [0 .. h `minus` 1], j >= y_nat, j < y_nat + rect_h_nat ]
    let indicesToUpdateI = [ i | i <- [0 .. w `minus` 1], i >= x_nat, i < x_nat + rect_w_nat ]

    for_ indicesToUpdateJ $ \j => do
      case natToFin j h of
        Nothing => pure ()
        Just fin_j => do
          let row = index fin_j grid
          for_ indicesToUpdateI $ \i => do
            case natToFin i w of
              Nothing => pure ()
              Just fin_i => do
                let current_cell = index fin_i row
                if current_cell == c
                  then pure ()
                  else do
                    let mov = moveCursor j i
                    let fg = ansifyColorFg tc (fg c)
                    let bg = ansifyColorBg tc (bg c)
                    let attr = ansifyAttr (attr c)
                    putStr (mov ++ fg ++ bg ++ attr ++ String.singleton (char c) ++ "\x1b[0m")

    -- Now compute new grid purely
    let updateRow = \row => foldl (\r, i => case natToFin i w of
                                              Nothing => r
                                              Just fin_i => Data.Vect.updateAt fin_i (const c) r) row indicesToUpdateI

    let grid' = foldl (\g, j => case natToFin j h of
                                  Nothing => g
                                  Just fin_j => Data.Vect.updateAt fin_j updateRow g) grid indicesToUpdateJ
    pure (MkTTY grid' tc)

  resize (MkTTY _ tc) new_w new_h = do
    putStr "\x1b[2J" -- clear screen sequence maybe? Wait, user said just clear surface on resize. We'll let the user decide if they want to clear it physically.
    pure (emptyTTY {w=new_w} {h=new_h} tc)
