{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( processFile
    , mkTextEncoding'
    , EncodingName
    ) where

import System.IO
import Control.Monad ( when )
import Data.Char ( toUpper )
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Csv as CSV
import Data.Vector (Vector)
import Data.Maybe ( fromMaybe )

type EncodingName = String

mkTextEncoding' :: EncodingName -> IO TextEncoding
mkTextEncoding' en =
  -- Если кодировка совпадает с одной из обязательно реализованных в стандартной
  -- библиотеке то выбираем ее явно, если нет, то используем mkTextEncoding
  case normalized of
    "ASCII"    -> return latin1
    "ISO88591" -> return latin1
    "LATIN1"   -> return latin1
    "UTF8"     -> return utf8
    "UTF16"    -> return utf16
    "UTF16LE"  -> return utf16le
    "UTF16BE"  -> return utf16be
    "UTF32LE"  -> return utf32le
    "UTF32BE"  -> return utf32be
    "WINDOWS1251" -> mkTextEncoding "CP1251"
    "WINDOWS866"  -> mkTextEncoding "CP866"
    _          -> mkTextEncoding (fmap toUpper en)

    where normalized = filter (`notElem` "_- ") (fmap toUpper en)

type Field = String
type Record = Vector Field
type Records = Vector Record

type CsvInput = String

recordElement = "r"
fieldElement = "f"

processFile :: String -> CsvInput -> Handle -> Maybe EncodingName -> IO ()
processFile sourceName csvInput outFileH outputEncoding = do
  case CSV.decode CSV.NoHeader (BLU.fromString csvInput) of
    Left err -> errorWithoutStackTrace err
    Right (r :: Records) -> do
      hSetBinaryMode outFileH True
      hSetEncoding outFileH =<< mkTextEncoding' outputEncoding'
      hPutStrLn outFileH $ makeDocument sourceName outputEncoding' r
  where
    outputEncoding' = fromMaybe "utf-8" outputEncoding

makeDocument :: String -> String -> Records -> String
makeDocument sourceName outputEncoding records =
    "<?xml version=\"1.0\" encoding=\"" <> outputEncoding <> "\"?>"
    <> "<csvPacket source=\"" <> escapeAttributeData sourceName <> "\">"
    <> mkRecords records
    <> "</csvPacket>"

mkRecords :: Records -> String
mkRecords = concatMap mkRecord

mkRecord :: Record -> String
mkRecord = mkElement recordElement . concatMap mkField

mkField :: Field -> String
mkField = mkElement fieldElement . escapeCharacterData

mkElement :: String -> String -> String
mkElement elementName elementContent =
    "<" <> elementName <> ">" <> elementContent <> "</" <> elementName <> ">"

escapeCharacterData :: String -> String
escapeCharacterData = concatMap $ \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    c   -> [c]

escapeAttributeData :: String -> [Char]
escapeAttributeData = escapeAttributeData' . escapeCharacterData
  where
    escapeAttributeData' = concatMap $ \case
        '"' -> "&quot;"
        c   -> [c]
