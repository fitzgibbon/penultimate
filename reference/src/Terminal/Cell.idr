module Terminal.Cell

import Terminal.Attr
import Terminal.Color

public export
record Cell where
  constructor MkCell
  ch : Char
  fg : Color
  bg : Color
  attrs : Attrs
  alpha : Int

export
defaultCell : Cell
defaultCell = MkCell ' ' (Named White) (Named Black) [] 0

export
Eq Cell where
  (MkCell ch1 fg1 bg1 attrs1 t1) == (MkCell ch2 fg2 bg2 attrs2 t2) =
    ch1 == ch2 && fg1 == fg2 && bg1 == bg2 && attrs1 == attrs2 && t1 == t2

export
withChar : Cell -> Char -> Cell
withChar cell value = { ch := value } cell
