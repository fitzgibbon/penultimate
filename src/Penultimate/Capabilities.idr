module Penultimate.Capabilities

import Data.String
import System

public export
data ColorTier = TrueColor | Ansi256 | Ansi16

public export
data RenderPolicy = PreferBest | ForceTrueColor | ForceAnsi256 | ForceAnsi16

public export
record Capabilities where
  constructor MkCapabilities
  supportsTrueColor : Bool
  supportsAnsi256 : Bool
  supportsAnsi16 : Bool

lowerString : String -> String
lowerString value = toLower value

envContains : String -> String -> IO Bool
envContains name needle = do
  val <- getEnv name
  case val of
    Nothing => pure False
    Just raw => pure (isInfixOf (lowerString needle) (lowerString raw))

export
detectCapabilities : IO Capabilities
detectCapabilities = do
  noColor <- getEnv "NO_COLOR"
  case noColor of
    Just _ => pure (MkCapabilities False False True)
    Nothing => do
      trueColorEnv <- envContains "COLORTERM" "truecolor"
      trueColorAlt <- envContains "COLORTERM" "24bit"
      trueColorTerm <- envContains "TERM" "direct"
      ansi256 <- envContains "TERM" "256color"
      let supportsTrue = trueColorEnv || trueColorAlt || trueColorTerm
      pure (MkCapabilities supportsTrue ansi256 True)

export
resolveTier : Capabilities -> RenderPolicy -> ColorTier
resolveTier caps policy =
  case policy of
    ForceTrueColor => TrueColor
    ForceAnsi256 => Ansi256
    ForceAnsi16 => Ansi16
    PreferBest =>
      if supportsTrueColor caps then TrueColor
      else if supportsAnsi256 caps then Ansi256
      else Ansi16
