{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( convertCsv2Xml
    , mkTextEncoding'
    , EncodingName
    , ConvertConfig (..)
    , Source
    , mkSourceFromFilePath
    , mkSourceFromStdin
    , mkDefaultConvertConfig
    ) where

import System.IO
import Control.Monad ( when )
import Data.Char ( toUpper, ord )
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Csv as CSV
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe ( fromMaybe )
import System.Directory (doesFileExist)
import System.PosixCompat.Files (getFileStatus, modificationTimeHiRes)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import Control.Monad.Reader (ReaderT, runReaderT, asks, ask)
import Control.Monad.Identity (Identity, runIdentity)

type EncodingName = String

data Source =
    Source
    {
      name :: String
    , fileName :: Maybe FilePath
    , timestamp :: Maybe ZonedTime
    }

mkSourceFromFilePath :: FilePath -> IO Source
mkSourceFromFilePath fileName =
    Source fileName (Just fileName) <$> getFileModificationTime fileName

mkSourceFromStdin :: Source
mkSourceFromStdin = Source "stdin" Nothing Nothing

data ConvertConfig =
    ConvertConfig
    {
      isTabDelimited :: Bool
    , isIndexedFieldNames :: Bool
    , recordElementName :: String
    , fieldElementName :: String
    , source :: Maybe Source
    , xmlNameSpace :: Maybe String
    }

mkDefaultConvertConfig :: ConvertConfig
mkDefaultConvertConfig =
    ConvertConfig
    { isTabDelimited = False
    , isIndexedFieldNames = False
    , recordElementName = "r"
    , fieldElementName = "f"
    , source = Nothing
    , xmlNameSpace = Nothing
    }

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

convertCsv2Xml :: ConvertConfig -> CsvInput -> Handle -> Maybe EncodingName -> IO ()
convertCsv2Xml convertConfig csvInput outFileH outputEncoding = do
  case CSV.decodeWith decodeOptions CSV.NoHeader (BLU.fromString csvInput) of
    Left err -> errorWithoutStackTrace err
    Right (r :: Records) -> do
      hSetBinaryMode outFileH True
      hSetEncoding outFileH =<< mkTextEncoding' outputEncoding'
      hPutStrLn outFileH $ runConverting convertConfig $ makeDocument outputEncoding' r
  where
    outputEncoding' = fromMaybe "utf-8" outputEncoding
    decodeOptions =
        let
            def = CSV.defaultDecodeOptions
        in
          if isTabDelimited convertConfig
              then def { CSV.decDelimiter = fromIntegral (ord '\t') }
              else def

type Converting a = ReaderT ConvertConfig Identity a

runConverting :: ConvertConfig -> Converting a -> a
runConverting convertConfig act =
    runIdentity $ runReaderT act convertConfig

makeDocument :: EncodingName -> Records -> Converting String
makeDocument outputEncoding records = do
    cfg <- ask
    records' <- mkRecords records
    return $
      "<?xml version=\"1.0\" encoding=\"" <> outputEncoding <> "\"?>"
      <> "<csvPacket" <> mkXmlns cfg <> mkSourceAttributes cfg <> ">"
      <> records'
      <> "</csvPacket>"
    where
      mkSourceAttributes cfg =
          case source cfg of
            Just s -> " source=\"" <> (escapeAttributeData . name) s <> "\""
                      <> case timestamp s of
                           Just t -> " timestamp=\"" <> (escapeAttributeData . toXsdDateTimeFormat) t <> "\""
                           Nothing -> ""
            Nothing -> ""
      mkXmlns cfg =
          case xmlNameSpace cfg of
            Just ns -> " xmlns=\"" <> escapeAttributeData ns <> "\""
            Nothing -> ""

mkRecords :: Records -> Converting String
mkRecords rs = concat <$> mapM mkRecord rs

mkRecord :: Record -> Converting String
mkRecord r =
  asks (mkElement . recordElementName) <*> mkFields r

mkFields :: Record -> Converting String
mkFields fs = do
    isIndexed <- asks isIndexedFieldNames
    fields <- if isIndexed
                  then mapM mkIndexedField (V.zip (V.fromList [1..]) fs)
                  else mapM mkField fs
    return $ concat fields

mkField :: Field -> Converting String
mkField f =
  asks (mkElement . fieldElementName) <*>  pure (escapeCharacterData f)

mkIndexedField :: (Integer, Field) -> Converting String
mkIndexedField (i, f) =
  asks (mkElement . (<> show i) . fieldElementName) <*> pure (escapeCharacterData f)

mkElement :: String -> String -> String
mkElement elementName elementContent =
    if null elementContent
        then "<" <> elementName <> "/>"
        else
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

getFileModificationTime :: FilePath -> IO (Maybe ZonedTime)
getFileModificationTime fileName = do
    isExists <- doesFileExist fileName
    if isExists
        then
            Just <$> (utcToLocalZonedTime . posixSecondsToUTCTime . modificationTimeHiRes =<< getFileStatus fileName)
        else
            return Nothing

toXsdDateTimeFormat :: ZonedTime -> String
toXsdDateTimeFormat t =
  let
      s = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%Q%z") t
      (prefix, suffix) = splitAt (length s -2) s
  in
    concat [prefix, ":", suffix]
