module Penultimate.Ansi

import Penultimate.Attr
import Penultimate.Color

esc : String -> String
esc code = "\x1b[" ++ code

export
cursorTo : Nat -> Nat -> String
cursorTo row col = esc (show row ++ ";" ++ show col ++ "H")

export
clearScreen : String
clearScreen = esc "2J"

export
hideCursor : String
hideCursor = esc "?25l"

export
showCursor : String
showCursor = esc "?25h"

export
resetAttrs : String
resetAttrs = esc "0m"

export
setAttr : Attr -> String
setAttr attr =
  case attr of
    Bold => esc "1m"
    Italic => esc "3m"
    Underline => esc "4m"

export
setFgAnsi16 : NamedColor -> String
setFgAnsi16 color = esc (show (fgCode color) ++ "m")
  where
    fgCode : NamedColor -> Int
    fgCode name =
      case name of
        Black => 30
        Red => 31
        Green => 32
        Yellow => 33
        Blue => 34
        Magenta => 35
        Cyan => 36
        White => 37
        BrightBlack => 90
        BrightRed => 91
        BrightGreen => 92
        BrightYellow => 93
        BrightBlue => 94
        BrightMagenta => 95
        BrightCyan => 96
        BrightWhite => 97

export
setBgAnsi16 : NamedColor -> String
setBgAnsi16 color = esc (show (bgCode color) ++ "m")
  where
    bgCode : NamedColor -> Int
    bgCode name =
      case name of
        Black => 40
        Red => 41
        Green => 42
        Yellow => 43
        Blue => 44
        Magenta => 45
        Cyan => 46
        White => 47
        BrightBlack => 100
        BrightRed => 101
        BrightGreen => 102
        BrightYellow => 103
        BrightBlue => 104
        BrightMagenta => 105
        BrightCyan => 106
        BrightWhite => 107

export
setFgAnsi256 : Int -> String
setFgAnsi256 idx = esc ("38;5;" ++ show idx ++ "m")

export
setBgAnsi256 : Int -> String
setBgAnsi256 idx = esc ("48;5;" ++ show idx ++ "m")

export
setFgTrueColor : RGB -> String
setFgTrueColor (MkRGB r g b) =
  esc ("38;2;" ++ show (channelValue r) ++ ";" ++ show (channelValue g) ++ ";" ++ show (channelValue b) ++ "m")

export
setBgTrueColor : RGB -> String
setBgTrueColor (MkRGB r g b) =
  esc ("48;2;" ++ show (channelValue r) ++ ";" ++ show (channelValue g) ++ ";" ++ show (channelValue b) ++ "m")
