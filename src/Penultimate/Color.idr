module Penultimate.Color

import Data.List
import Penultimate.Capabilities

public export
record Channel where
  constructor MkChannel
  value : Int

public export
record RGB where
  constructor MkRGB
  r : Channel
  g : Channel
  b : Channel

public export
record RGBA where
  constructor MkRGBA
  r : Channel
  g : Channel
  b : Channel
  a : Channel

public export
data NamedColor
  = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightBlack | BrightRed | BrightGreen | BrightYellow
  | BrightBlue | BrightMagenta | BrightCyan | BrightWhite

public export
record UserColor where
  constructor MkUserColor
  trueColor : Maybe RGB
  ansi256 : Maybe Int
  ansi16 : Maybe NamedColor

public export
data Color
  = Named NamedColor
  | RGBColor RGB
  | Ansi256Color Int
  | Ansi16Color NamedColor
  | Custom UserColor

export
Eq Channel where
  (MkChannel v1) == (MkChannel v2) = v1 == v2

export
Eq RGB where
  (MkRGB r1 g1 b1) == (MkRGB r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

export
Eq RGBA where
  (MkRGBA r1 g1 b1 a1) == (MkRGBA r2 g2 b2 a2) =
    r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2

export
Eq NamedColor where
  Black == Black = True
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  Blue == Blue = True
  Magenta == Magenta = True
  Cyan == Cyan = True
  White == White = True
  BrightBlack == BrightBlack = True
  BrightRed == BrightRed = True
  BrightGreen == BrightGreen = True
  BrightYellow == BrightYellow = True
  BrightBlue == BrightBlue = True
  BrightMagenta == BrightMagenta = True
  BrightCyan == BrightCyan = True
  BrightWhite == BrightWhite = True
  _ == _ = False

export
Eq UserColor where
  (MkUserColor tc1 a1 s1) == (MkUserColor tc2 a2 s2) = tc1 == tc2 && a1 == a2 && s1 == s2

export
Eq Color where
  (Named c1) == (Named c2) = c1 == c2
  (RGBColor c1) == (RGBColor c2) = c1 == c2
  (Ansi256Color c1) == (Ansi256Color c2) = c1 == c2
  (Ansi16Color c1) == (Ansi16Color c2) = c1 == c2
  (Custom c1) == (Custom c2) = c1 == c2
  _ == _ = False

clamp : Int -> Int
clamp value = if value < 0 then 0 else if value > 255 then 255 else value

export
mkChannel : Int -> Channel
mkChannel value = MkChannel (clamp value)

export
channelValue : Channel -> Int
channelValue (MkChannel value) = value

export
mkRGB : Int -> Int -> Int -> RGB
mkRGB r g b = MkRGB (mkChannel r) (mkChannel g) (mkChannel b)

export
mkRGBA : Int -> Int -> Int -> Int -> RGBA
mkRGBA r g b a = MkRGBA (mkChannel r) (mkChannel g) (mkChannel b) (mkChannel a)

ansi16Palette : List (NamedColor, RGB)
ansi16Palette =
  [ (Black, mkRGB 0 0 0)
  , (Red, mkRGB 205 0 0)
  , (Green, mkRGB 0 205 0)
  , (Yellow, mkRGB 205 205 0)
  , (Blue, mkRGB 0 0 238)
  , (Magenta, mkRGB 205 0 205)
  , (Cyan, mkRGB 0 205 205)
  , (White, mkRGB 229 229 229)
  , (BrightBlack, mkRGB 127 127 127)
  , (BrightRed, mkRGB 255 0 0)
  , (BrightGreen, mkRGB 0 255 0)
  , (BrightYellow, mkRGB 255 255 0)
  , (BrightBlue, mkRGB 92 92 255)
  , (BrightMagenta, mkRGB 255 0 255)
  , (BrightCyan, mkRGB 0 255 255)
  , (BrightWhite, mkRGB 255 255 255)
  ]

export
namedToRGB : NamedColor -> RGB
namedToRGB color =
  case lookup color ansi16Palette of
    Just rgb => rgb
    Nothing => mkRGB 0 0 0

export
ansi16ToRGB : NamedColor -> RGB
ansi16ToRGB = namedToRGB

rgbDistance : RGB -> RGB -> Int
rgbDistance (MkRGB r1 g1 b1) (MkRGB r2 g2 b2) =
  let dr = channelValue r1 - channelValue r2
      dg = channelValue g1 - channelValue g2
      db = channelValue b1 - channelValue b2
   in dr * dr + dg * dg + db * db

closestAnsi16 : RGB -> NamedColor
closestAnsi16 rgb =
  let pick : (NamedColor, RGB) -> (NamedColor, RGB) -> (NamedColor, RGB)
      pick a b = if rgbDistance rgb (snd a) <= rgbDistance rgb (snd b) then a else b
   in fst (foldl pick (Black, mkRGB 0 0 0) ansi16Palette)

ansi256Levels : List Int
ansi256Levels = [0, 95, 135, 175, 215, 255]

ansi256ToRGB : Int -> RGB
ansi256ToRGB idx =
  if idx < 0 then mkRGB 0 0 0
  else if idx < 16 then
    case lookup (indexAnsi16 idx) ansi16Palette of
      Just rgb => rgb
      Nothing => mkRGB 0 0 0
  else if idx < 232 then
    let n = idx - 16
        rIdx = div n 36
        gIdx = div (n `mod` 36) 6
        bIdx = n `mod` 6
        levelAt : Int -> Int
        levelAt i =
          case i of
            0 => 0
            1 => 95
            2 => 135
            3 => 175
            4 => 215
            _ => 255
     in mkRGB (levelAt rIdx) (levelAt gIdx) (levelAt bIdx)
  else if idx < 256 then
    let level = 8 + (idx - 232) * 10
     in mkRGB level level level
  else mkRGB 0 0 0
  where
    indexAnsi16 : Int -> NamedColor
    indexAnsi16 value =
      case value of
        0 => Black
        1 => Red
        2 => Green
        3 => Yellow
        4 => Blue
        5 => Magenta
        6 => Cyan
        7 => White
        8 => BrightBlack
        9 => BrightRed
        10 => BrightGreen
        11 => BrightYellow
        12 => BrightBlue
        13 => BrightMagenta
        14 => BrightCyan
        _ => BrightWhite

nearestLevelIndex : Int -> Int
nearestLevelIndex value =
  let candidates = zip ansi256Levels [0, 1, 2, 3, 4, 5]
      pick : (Int, Int) -> (Int, Int) -> (Int, Int)
      pick a b = if abs (fst a - value) <= abs (fst b - value) then a else b
   in snd (foldl pick (0, 0) candidates)

rgbToAnsi256 : RGB -> Int
rgbToAnsi256 (MkRGB r g b) =
  let rVal = channelValue r
      gVal = channelValue g
      bVal = channelValue b
      rIdx = nearestLevelIndex rVal
      gIdx = nearestLevelIndex gVal
      bIdx = nearestLevelIndex bVal
      cubeIdx = 16 + (36 * rIdx) + (6 * gIdx) + bIdx
      gray = (rVal + gVal + bVal) `div` 3
      grayIdx =
        if gray < 8 then 232
        else if gray > 238 then 255
        else 232 + ((gray - 8) `div` 10)
      cubeRGB = ansi256ToRGB cubeIdx
      grayRGB = ansi256ToRGB grayIdx
   in if rgbDistance (MkRGB r g b) grayRGB < rgbDistance (MkRGB r g b) cubeRGB
        then grayIdx
        else cubeIdx

export
colorToRGB : Color -> RGB
colorToRGB value =
  case value of
    Named name => namedToRGB name
    RGBColor rgb => rgb
    Ansi256Color idx => ansi256ToRGB idx
    Ansi16Color name => ansi16ToRGB name
    Custom user =>
      case user.trueColor of
        Just rgb => rgb
        Nothing =>
          case user.ansi256 of
            Just idx => ansi256ToRGB idx
            Nothing =>
              case user.ansi16 of
                Just name => ansi16ToRGB name
                Nothing => mkRGB 0 0 0

export
resolveColor : ColorTier -> Color -> Color
resolveColor tier color =
  case tier of
    TrueColor => RGBColor (colorToRGB color)
    Ansi256 => Ansi256Color (toAnsi256 color)
    Ansi16 => Ansi16Color (toAnsi16 color)
  where
    toAnsi256 : Color -> Int
    toAnsi256 value =
      case value of
        Named name => rgbToAnsi256 (namedToRGB name)
        RGBColor rgb => rgbToAnsi256 rgb
        Ansi256Color idx => idx
        Ansi16Color name => rgbToAnsi256 (ansi16ToRGB name)
        Custom user =>
          case user.ansi256 of
            Just idx => idx
            Nothing =>
              case user.trueColor of
                Just rgb => rgbToAnsi256 rgb
                Nothing =>
                  case user.ansi16 of
                    Just name => rgbToAnsi256 (ansi16ToRGB name)
                    Nothing => 0

    toAnsi16 : Color -> NamedColor
    toAnsi16 value =
      case value of
        Named name => name
        RGBColor rgb => closestAnsi16 rgb
        Ansi256Color idx => closestAnsi16 (ansi256ToRGB idx)
        Ansi16Color name => name
        Custom user =>
          case user.ansi16 of
            Just name => name
            Nothing =>
              case user.trueColor of
                Just rgb => closestAnsi16 rgb
                Nothing =>
                  case user.ansi256 of
                    Just idx => closestAnsi16 (ansi256ToRGB idx)
                    Nothing => Black
