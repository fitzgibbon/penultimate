module Terminal.Input

import Data.List
import Data.String
import System
import System.File.Error
import System.File.ReadWrite
import System.File.Types
import System.File.Virtual

%foreign "C:idris2_fpoll, libidris2_support, idris_file.h"
prim__fpoll : FilePtr -> PrimIO Int

public export
record Modifiers where
  constructor MkModifiers
  shift : Bool
  alt : Bool
  ctrl : Bool

public export
data SpecialKey
  = ArrowUp | ArrowDown | ArrowLeft | ArrowRight
  | Home | End | PageUp | PageDown | Insert | Delete
  | FunctionKey Int | EscapeKey

public export
data Key
  = KeyChar Char Modifiers
  | KeySpecial SpecialKey Modifiers

export
noModifiers : Modifiers
noModifiers = MkModifiers False False False

export
enableRaw : HasIO io => io Bool
enableRaw = do
  result <- enableRawMode
  case result of
    Right _ => pure True
    Left _ => pure False

export
disableRaw : HasIO io => io Bool
disableRaw = do
  resetRawMode
  pure True

modifiersFromCode : Int -> Modifiers
modifiersFromCode code =
  case code of
    2 => MkModifiers True False False
    3 => MkModifiers False True False
    4 => MkModifiers True True False
    5 => MkModifiers False False True
    6 => MkModifiers True False True
    7 => MkModifiers False True True
    8 => MkModifiers True True True
    _ => noModifiers

parseParams : String -> List Int
parseParams raw = parseParamsChars (unpack raw) [] []
  where
    parseParam : List Char -> Int
    parseParam value =
      case parseInteger (pack value) of
        Just num => cast num
        Nothing => 0

    parseParamsChars : List Char -> List Char -> List Int -> List Int
    parseParamsChars [] current acc = reverse (parseParam current :: acc)
    parseParamsChars (';' :: rest) current acc =
      parseParamsChars rest [] (parseParam current :: acc)
    parseParamsChars (ch :: rest) current acc =
      parseParamsChars rest (current ++ [ch]) acc

isFinal : Char -> Bool
isFinal ch =
  ch == 'A' || ch == 'B' || ch == 'C' || ch == 'D' ||
  ch == 'H' || ch == 'F' || ch == '~'

collect : String -> IO String
collect acc = do
  ch <- getChar
  if isFinal ch then pure (acc ++ singleton ch) else collect (acc ++ singleton ch)

initParams : String -> String
initParams seq =
  case reverse (unpack seq) of
    [] => ""
    _ :: rest => pack (reverse rest)

lastChar : String -> Char
lastChar seq =
  case reverse (unpack seq) of
    [] => ' '
    ch :: _ => ch

decodeTilde : List Int -> Modifiers -> Key
decodeTilde params mods =
  case params of
    1 :: _ => KeySpecial Home mods
    2 :: _ => KeySpecial Insert mods
    3 :: _ => KeySpecial Delete mods
    4 :: _ => KeySpecial End mods
    5 :: _ => KeySpecial PageUp mods
    6 :: _ => KeySpecial PageDown mods
    15 :: _ => KeySpecial (FunctionKey 5) mods
    17 :: _ => KeySpecial (FunctionKey 6) mods
    18 :: _ => KeySpecial (FunctionKey 7) mods
    19 :: _ => KeySpecial (FunctionKey 8) mods
    20 :: _ => KeySpecial (FunctionKey 9) mods
    21 :: _ => KeySpecial (FunctionKey 10) mods
    23 :: _ => KeySpecial (FunctionKey 11) mods
    24 :: _ => KeySpecial (FunctionKey 12) mods
    _ => KeySpecial EscapeKey mods

decodeCsi : Char -> List Int -> Modifiers -> Key
decodeCsi code params mods =
  case code of
    'A' => KeySpecial ArrowUp mods
    'B' => KeySpecial ArrowDown mods
    'C' => KeySpecial ArrowRight mods
    'D' => KeySpecial ArrowLeft mods
    'H' => KeySpecial Home mods
    'F' => KeySpecial End mods
    '~' => decodeTilde params mods
    _ => KeySpecial EscapeKey mods

readCsi : IO Key
readCsi = do
  seq <- collect ""
  let params = parseParams (initParams seq)
  case params of
    _ :: modCode :: _ => pure (decodeCsi (lastChar seq) params (modifiersFromCode modCode))
    _ => pure (decodeCsi (lastChar seq) params noModifiers)

readFunction : IO Key
readFunction = do
  key <- getChar
  case key of
    'P' => pure (KeySpecial (FunctionKey 1) noModifiers)
    'Q' => pure (KeySpecial (FunctionKey 2) noModifiers)
    'R' => pure (KeySpecial (FunctionKey 3) noModifiers)
    'S' => pure (KeySpecial (FunctionKey 4) noModifiers)
    _ => pure (KeySpecial EscapeKey noModifiers)

readEscape : IO Key
readEscape = do
  next <- getChar
  case next of
    '[' => readCsi
    'O' => readFunction
    _ => pure (KeyChar next ({ alt := True } noModifiers))

export
readKey : IO Key
readKey = do
  ch <- getChar
  if ch == '\x1b' then readEscape else pure (KeyChar ch noModifiers)

readCharMaybe : IO (Maybe Char)
readCharMaybe =
  case stdin of
    FHandle handle => do
      ready <- primIO (prim__fpoll handle)
      if ready == 0
         then pure Nothing
         else do
           result <- fGetChar stdin
           case result of
             Right ch => pure (Just ch)
             Left _ => pure Nothing

collectMaybe : String -> IO String
collectMaybe acc = do
  next <- readCharMaybe
  case next of
    Nothing => pure acc
    Just ch => if isFinal ch then pure (acc ++ singleton ch) else collectMaybe (acc ++ singleton ch)

readCsiMaybe : IO Key
readCsiMaybe = do
  seq <- collectMaybe ""
  let params = parseParams (initParams seq)
  case params of
    _ :: modCode :: _ => pure (decodeCsi (lastChar seq) params (modifiersFromCode modCode))
    _ => pure (decodeCsi (lastChar seq) params noModifiers)

readFunctionMaybe : IO Key
readFunctionMaybe = do
  key <- readCharMaybe
  case key of
    Just 'P' => pure (KeySpecial (FunctionKey 1) noModifiers)
    Just 'Q' => pure (KeySpecial (FunctionKey 2) noModifiers)
    Just 'R' => pure (KeySpecial (FunctionKey 3) noModifiers)
    Just 'S' => pure (KeySpecial (FunctionKey 4) noModifiers)
    _ => pure (KeySpecial EscapeKey noModifiers)

readEscapeMaybe : IO Key
readEscapeMaybe = do
  next <- readCharMaybe
  case next of
    Just '[' => readCsiMaybe
    Just 'O' => readFunctionMaybe
    Just ch => pure (KeyChar ch ({ alt := True } noModifiers))
    Nothing => pure (KeySpecial EscapeKey noModifiers)

export
readKeyMaybe : IO (Maybe Key)
readKeyMaybe = do
  first <- readCharMaybe
  case first of
    Nothing => pure Nothing
    Just ch =>
      if ch == '\x1b'
         then do key <- readEscapeMaybe; pure (Just key)
         else pure (Just (KeyChar ch noModifiers))
