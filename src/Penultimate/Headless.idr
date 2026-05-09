module Penultimate.Headless

import Data.Fin
import Data.Nat
import Data.Vect
import Penultimate.Core
import Control.Monad.Identity

public export
record HeadlessSurface (w : Nat) (h : Nat) where
  constructor MkHeadless
  grid : Vect h (Vect w StyledChar)

public export
emptyHeadless : {w : Nat} -> {h : Nat} -> HeadlessSurface w h
emptyHeadless = MkHeadless (replicate h (replicate w defaultStyledChar))

updateRectRow : {w : Nat} -> Nat -> Nat -> StyledChar -> Vect w StyledChar -> Vect w StyledChar
updateRectRow x_nat rect_w_nat c row =
  let indicesToUpdate = [ i | i <- [0 .. w `minus` 1], i >= x_nat, i < x_nat + rect_w_nat ] in
      -- Convert Nats back to Fins
      foldl (\r, i => case natToFin i w of
                        Nothing => r
                        Just fin_i => Data.Vect.updateAt fin_i (const c) r) row indicesToUpdate

updateGrid : {w : Nat} -> {h : Nat} -> Nat -> Nat -> Nat -> Nat -> StyledChar -> Vect h (Vect w StyledChar) -> Vect h (Vect w StyledChar)
updateGrid x_nat y_nat rect_w_nat rect_h_nat c grid =
  let indicesToUpdate = [ j | j <- [0 .. h `minus` 1], j >= y_nat, j < y_nat + rect_h_nat ] in
      foldl (\g, j => case natToFin j h of
                        Nothing => g
                        Just fin_j => Data.Vect.updateAt fin_j (updateRectRow x_nat rect_w_nat c) g) grid indicesToUpdate

public export
Surface HeadlessSurface Identity where
  drawRect (MkHeadless grid) x y rect_w rect_h c =
    let x_nat = finToNat x
        y_nat = finToNat y
        rect_w_nat = finToNat rect_w
        rect_h_nat = finToNat rect_h
        grid' = updateGrid x_nat y_nat rect_w_nat rect_h_nat c grid
    in Id (MkHeadless grid')

  resize _ new_w new_h = Id (emptyHeadless {w=new_w} {h=new_h})
