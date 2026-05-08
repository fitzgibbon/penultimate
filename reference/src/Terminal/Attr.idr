module Terminal.Attr

public export
data Attr = Bold | Italic | Underline

public export
Attrs : Type
Attrs = List Attr

export
Eq Attr where
  Bold == Bold = True
  Italic == Italic = True
  Underline == Underline = True
  _ == _ = False
