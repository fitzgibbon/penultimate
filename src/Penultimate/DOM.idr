module Penultimate.DOM

import Data.Fin
import Data.Nat
import Data.Vect
import Penultimate.Core
import Data.String

%foreign "browser:lambda:()=>document.getElementById('penultimate-root')"
getRootElement : IO AnyPtr

%foreign "browser:lambda:(root)=>root.innerHTML = ''"
clearElement : AnyPtr -> IO ()

%foreign "browser:lambda:(r, c)=>{ let el = document.createElement('span'); el.id = 'cell-' + r + '-' + c; return el; }"
createSpan : Nat -> Nat -> IO AnyPtr

%foreign "browser:lambda:(root, el)=>root.appendChild(el)"
appendChild : AnyPtr -> AnyPtr -> IO ()

%foreign "browser:lambda:()=>document.createElement('br')"
createBr : IO AnyPtr

%foreign "browser:lambda:(r, c)=>{ return document.getElementById('cell-' + r + '-' + c); }"
getSpan : Nat -> Nat -> IO AnyPtr

%foreign "browser:lambda:(el, txt)=>el.textContent = txt"
setTextContent : AnyPtr -> String -> IO ()

%foreign "browser:lambda:(el, fg, bg, fontWeight, textDecoration, fontStyle)=>{ el.style.color = fg; el.style.backgroundColor = bg; el.style.fontWeight = fontWeight; el.style.textDecoration = textDecoration; el.style.fontStyle = fontStyle; }"
setStyle : AnyPtr -> String -> String -> String -> String -> String -> IO ()

public export
record DOMSurface (w : Nat) (h : Nat) where
  constructor MkDOM
  grid : Vect h (Vect w StyledChar)

emptyDOM : {w : Nat} -> {h : Nat} -> DOMSurface w h
emptyDOM = MkDOM
  (replicate h (replicate w defaultStyledChar))

colorToCSS : Color -> String
colorToCSS (Named n) =
  case n of
    0 => "black"
    1 => "red"
    2 => "green"
    3 => "yellow"
    4 => "blue"
    5 => "magenta"
    6 => "cyan"
    7 => "white"
    8 => "gray"
    9 => "lightcoral"
    10 => "lightgreen"
    11 => "lightyellow"
    12 => "lightblue"
    13 => "lightpink"
    14 => "lightcyan"
    15 => "white"
    _ => "black"
colorToCSS (RGB r g b) = "rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

attrToCSS : Attr -> (String, String, String)
attrToCSS (MkAttr b u i) =
  let fontWeight = if b then "bold" else "normal"
      textDecoration = if u then "underline" else "none"
      fontStyle = if i then "italic" else "normal"
  in (fontWeight, textDecoration, fontStyle)

public export
initDOM : {w : Nat} -> {h : Nat} -> IO (DOMSurface w h)
initDOM = do
  root <- getRootElement
  clearElement root

  for_ [0 .. (h `minus` 1)] $ \r => do
    for_ [0 .. (w `minus` 1)] $ \c => do
      span <- createSpan r c
      let (fw, td, fs) = attrToCSS defaultAttr
      setStyle span (colorToCSS (Named 7)) (colorToCSS (Named 0)) fw td fs
      setTextContent span " "
      appendChild root span
    br <- createBr
    appendChild root br

  pure emptyDOM

public export
Surface DOMSurface IO where
  drawRect (MkDOM grid) x y rect_w rect_h c = do
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
                    span <- getSpan j i
                    let (fw, td, fs) = attrToCSS (attr c)
                    setStyle span (colorToCSS (fg c)) (colorToCSS (bg c)) fw td fs
                    setTextContent span (singleton (char c))

    let updateRow = \row => foldl (\r, i => case natToFin i w of
                                              Nothing => r
                                              Just fin_i => Data.Vect.updateAt fin_i (const c) r) row indicesToUpdateI

    let grid' = foldl (\g, j => case natToFin j h of
                                  Nothing => g
                                  Just fin_j => Data.Vect.updateAt fin_j updateRow g) grid indicesToUpdateJ
    pure (MkDOM grid')

  resize _ new_w new_h = do
    root <- getRootElement
    clearElement root
    for_ [0 .. (new_h `minus` 1)] $ \r => do
      for_ [0 .. (new_w `minus` 1)] $ \c => do
        span <- createSpan r c
        let (fw, td, fs) = attrToCSS defaultAttr
        setStyle span (colorToCSS (Named 7)) (colorToCSS (Named 0)) fw td fs
        setTextContent span " "
        appendChild root span
      br <- createBr
      appendChild root br
    pure emptyDOM
