module Penultimate.InputBackend

import Data.List
import Data.String
import Penultimate.Backend
import Penultimate.Input

collect : TerminalBackend m => String -> m String
collect acc = do
  ch <- readChar
  if isFinal ch then pure (acc ++ cast ch) else collect (acc ++ cast ch)

readCsi : TerminalBackend m => m Key
readCsi = do
  seq <- collect ""
  let params = parseParams (initParams seq)
  case params of
    _ :: modCode :: _ => pure (decodeCsi (lastChar seq) params (modifiersFromCode modCode))
    _ => pure (decodeCsi (lastChar seq) params noModifiers)

readFunction : TerminalBackend m => m Key
readFunction = do
  key <- readChar
  case key of
    'P' => pure (KeySpecial (FunctionKey 1) noModifiers)
    'Q' => pure (KeySpecial (FunctionKey 2) noModifiers)
    'R' => pure (KeySpecial (FunctionKey 3) noModifiers)
    'S' => pure (KeySpecial (FunctionKey 4) noModifiers)
    _ => pure (KeySpecial EscapeKey noModifiers)

readEscape : TerminalBackend m => m Key
readEscape = do
  next <- readChar
  case next of
    '[' => readCsi
    'O' => readFunction
    _ => pure (KeyChar next ({ alt := True } noModifiers))

export
readKey : TerminalBackend m => m Key
readKey = do
  ch <- readChar
  if ch == '\x1b' then readEscape else pure (KeyChar ch noModifiers)

collectMaybe : TerminalBackend m => String -> m String
collectMaybe acc = do
  next <- pollChar
  case next of
    Nothing => pure acc
    Just ch => if isFinal ch then pure (acc ++ cast ch) else collectMaybe (acc ++ cast ch)

readCsiMaybe : TerminalBackend m => m Key
readCsiMaybe = do
  seq <- collectMaybe ""
  let params = parseParams (initParams seq)
  case params of
    _ :: modCode :: _ => pure (decodeCsi (lastChar seq) params (modifiersFromCode modCode))
    _ => pure (decodeCsi (lastChar seq) params noModifiers)

readFunctionMaybe : TerminalBackend m => m Key
readFunctionMaybe = do
  key <- pollChar
  case key of
    Just 'P' => pure (KeySpecial (FunctionKey 1) noModifiers)
    Just 'Q' => pure (KeySpecial (FunctionKey 2) noModifiers)
    Just 'R' => pure (KeySpecial (FunctionKey 3) noModifiers)
    Just 'S' => pure (KeySpecial (FunctionKey 4) noModifiers)
    _ => pure (KeySpecial EscapeKey noModifiers)

readEscapeMaybe : TerminalBackend m => m Key
readEscapeMaybe = do
  next <- pollChar
  case next of
    Just '[' => readCsiMaybe
    Just 'O' => readFunctionMaybe
    Just ch => pure (KeyChar ch ({ alt := True } noModifiers))
    Nothing => pure (KeySpecial EscapeKey noModifiers)

export
readKeyMaybe : TerminalBackend m => m (Maybe Key)
readKeyMaybe = do
  first <- pollChar
  case first of
    Nothing => pure Nothing
    Just ch =>
      if ch == '\x1b'
         then do key <- readEscapeMaybe; pure (Just key)
         else pure (Just (KeyChar ch noModifiers))
