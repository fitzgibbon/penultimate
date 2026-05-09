module Penultimate.BrowserBackend

import Data.Fin
import Data.Vect
import Data.So
import Penultimate.Backend
import Penultimate.Capabilities
import Penultimate.Ansi
import Penultimate.Color
import Data.String

%foreign "javascript:lambda: () => window.penultimate_readChar()"
prim__readChar : PrimIO Char

%foreign "javascript:lambda: () => { let c = window.penultimate_pollChar(); return c === null ? 0 : c.charCodeAt(0); }"
prim__pollCharInt : PrimIO Int

%foreign "javascript:lambda: (r, c, fg, bg, ch) => window.penultimate_drawChar(r, c, fg, bg, ch)"
prim__drawCharDOM : Int -> Int -> String -> String -> String -> PrimIO ()

%foreign "javascript:lambda: () => window.penultimate_initDOM()"
prim__initDOM : PrimIO ()

%foreign "javascript:lambda: () => window.penultimate_shutdownDOM()"
prim__shutdownDOM : PrimIO ()

%foreign "javascript:lambda: () => window.penultimate_clearDOM()"
prim__clearDOM : PrimIO ()

safeReadChar : IO Char
safeReadChar = primIO prim__readChar

safePollChar : IO (Maybe Char)
safePollChar = do
  code <- primIO prim__pollCharInt
  if code == 0 then pure Nothing else pure (Just (cast code))

hexChar : Int -> Char
hexChar 0 = '0'
hexChar 1 = '1'
hexChar 2 = '2'
hexChar 3 = '3'
hexChar 4 = '4'
hexChar 5 = '5'
hexChar 6 = '6'
hexChar 7 = '7'
hexChar 8 = '8'
hexChar 9 = '9'
hexChar 10 = 'A'
hexChar 11 = 'B'
hexChar 12 = 'C'
hexChar 13 = 'D'
hexChar 14 = 'E'
hexChar 15 = 'F'
hexChar _ = '0'

toHex : Int -> String
toHex val =
  let hi = hexChar (val `div` 16)
      lo = hexChar (val `mod` 16)
  in cast hi ++ cast lo

colorToCSS : Color -> String
colorToCSS c = case colorToRGB c of
  MkRGB r g b => "#" ++ toHex (channelValue r) ++ toHex (channelValue g) ++ toHex (channelValue b)

drawRectDOM : Nat -> Nat -> List (List StyledChar) -> IO ()
drawRectDOM _ _ [] = pure ()
drawRectDOM r c (line :: rest) = do
  let emitChar : Nat -> StyledChar -> IO ()
      emitChar colOffset sc = do
        let fgCSS = colorToCSS sc.style.fgStyle
        let bgCSS = colorToCSS sc.style.bgStyle
        primIO (prim__drawCharDOM (cast r) (cast (c + colOffset)) fgCSS bgCSS (singleton sc.char))

  -- Create indices
  let goLine : Nat -> List StyledChar -> IO ()
      goLine _ [] = pure ()
      goLine off (x :: xs) = do
        emitChar off x
        goLine (off + 1) xs

  goLine 0 line
  drawRectDOM (r + 1) c rest

export
TerminalBackend IO where
  initBackend = primIO prim__initDOM
  shutdownBackend = primIO prim__shutdownDOM
  clearScreen = primIO prim__clearDOM
  drawRect r c rect _ _ = drawRectDOM (finToNat r) (finToNat c) (map toList (toList rect))
  flush = pure ()
  readChar = safeReadChar
  pollChar = safePollChar
  getSize = pure (24, 80)
  resizePending = pure False
  getCapabilities = pure (MkCapabilities True True True)
  sleep ms = pure ()
