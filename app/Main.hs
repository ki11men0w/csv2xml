{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Environment (getProgName)
import System.Console.CmdArgs
import System.IO (IOMode (ReadMode), stdin, stdout, openFile, hGetContents, hSetEncoding, hSetBinaryMode, hClose)
import System.IO.Temp (withSystemTempFile)
import System.Directory (copyFile)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Paths_csv2xml (version)
import Data.Version (showVersion)

programVersion :: String
programVersion =
  showVersion version ++ " (haskell)"

data Flags =
    Flags
    { input_encoding :: Maybe EncodingName
    , output_encoding :: Maybe EncodingName
    , record_element_name :: Maybe String
    , field_element_name :: Maybe String
    , is_indexed_field_names :: Bool
    , xml_namespace :: Maybe String
    , tab_delimited :: Bool
    , store_records_source :: Bool
    , file_names :: [FilePath]
    } deriving (Data, Typeable)

optsDefinition :: IO Flags
optsDefinition = getProgName >>= \programName -> return $
       Flags { input_encoding =
                   def
                   &= help "Encoding of input CSV file. If not specified then default system encoding will be used"
                   &= explicit &= name "i" &= name "input-encoding"
                   &= typ "ENC"
             , output_encoding =
                   def
                   &= help "Encoding of resulting XML file. If not specified then UTF-8 will be used as default"
                   &= explicit &= name "e" &= name "o" &= name "output-encoding"
                   &= typ "ENC"
             , is_indexed_field_names =
                 def
                 &= help "By default all field elements are created with the same name. Use this option to add index number suffix to the field names"
                 &= explicit &= name "x" &= name "indexed-fields"
             , record_element_name =
                 def
                 &= help "Name for the record XML elements. If not specified the name of `r` will be used"
                 &= explicit &= name "r" &= name "record-tag-name"
                 &= typ "TAG_NAME"
             , field_element_name =
                 def
                 &= help "Name for the field XML elements. If not specified the name of `f` will be used"
                 &= explicit &= name "f" &= name "field-tag-name"
                 &= typ "TAG_NAME"
             , xml_namespace =
                 def
                 &= help "XML namespace for all elements of the output XML document. By default no namespace is used."
                 &= explicit &= name "n" &= name "namespace"
                 &= typ "XML_NAME_SPACE"
             , tab_delimited =
                 def
                 &= help "Specify this option if the fields in the input CSV file are separated by a tab character. By default, the fields are separated by a comma."
                 &= explicit &= name "t" &= name "tab-delimited"
             , store_records_source =
                 def
                 &= help "If specified then every record will be acomplished with extra `sourceRecord` element that will contain original CSV record text"
             , file_names =
                 def &= args &= typ "INPUT_FILE [OUTPUT_FILE]"
             }
       &= program programName
       &= summary ("CSV to XML converter version " <> programVersion)
       &= details [ "Converts CSV file to XML representation."
                  , "If you do not specify input/output file names on the command line then STDIN/STDOUT will be used. You also can explicitly specify to use standard input and output if specify a dash (\"-\") as file name."
                  ]

checkOptions :: Flags -> IO Flags
checkOptions flags =
    return $ flags { file_names = correctedFileNames }
    where
      files = file_names flags
      correctedFileNames =
          case length files of
            0 -> ["-", "-"]
            1 -> files <> ["-"]
            2 -> files
            _ -> errorWithoutStackTrace "Too many arguments on command line specified"


main :: IO ()
main = do
  opts <- optsDefinition >>= cmdArgs >>= checkOptions
  let
    [inFileName, outFileName] = file_names opts
    inputEncoding = input_encoding opts
    outputEncoding = output_encoding opts

  (inFileH, source) <-
      if inFileName == "-"
          then return (stdin, mkSourceFromStdin)
          else openFile inFileName ReadMode >>= \h -> mkSourceFromFilePath inFileName >>= \s ->  return (h, s)
  hSetBinaryMode inFileH False
  when (isJust inputEncoding) $
       hSetEncoding inFileH =<< mkTextEncoding' (fromJust inputEncoding)
  inFileContent <- hGetContents inFileH

  let
      convertOptions = flagsToConvertConfig opts source

  if outFileName == "-"
      then do
          convertCsv2Xml convertOptions inFileContent stdout outputEncoding
      else
          withSystemTempFile "csv2xml.xml" $ \tmpFileP tmpFileH -> do
            convertCsv2Xml convertOptions inFileContent tmpFileH outputEncoding
            hClose inFileH
            hClose tmpFileH
            copyFile tmpFileP outFileName

flagsToConvertConfig :: Flags -> Source -> ConvertConfig
flagsToConvertConfig flags source =
    mkDefaultConvertConfig
    { isTabDelimited = tab_delimited flags
    , isIndexedFieldNames = is_indexed_field_names flags
    , recordElementName = fromMaybe "r" $ record_element_name flags
    , fieldElementName = fromMaybe "f" $ field_element_name flags
    , xmlNameSpace = xml_namespace flags
    , source = Just source
    , storeRecordsSource = store_records_source flags
    }
