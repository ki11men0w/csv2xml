{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Environment (getProgName)
import System.Console.CmdArgs
import System.IO (IOMode (ReadMode), stdin, stdout, openFile, hGetContents, hSetEncoding, hSetBinaryMode)
import System.IO.Temp (withSystemTempFile)
import System.Directory (copyFile)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)

import Paths_csv2xml (version)
import Data.Version (showVersion)

programVersion :: String
programVersion =
  showVersion version ++ " (haskell)"

data Flags =
    Flags
    { input_encoding :: Maybe EncodingName
    , output_encoding :: Maybe EncodingName
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

  (inFileH, sourceName) <-
      if inFileName == "-"
          then return (stdin, "stdin")
          else openFile inFileName ReadMode >>= \h -> return (h, inFileName)
  hSetBinaryMode inFileH True
  when (isJust inputEncoding) $
       hSetEncoding inFileH =<< mkTextEncoding' (fromJust inputEncoding)
  inFileContent <- hGetContents inFileH

  if outFileName == "-"
      then do
          processFile sourceName inFileContent stdout outputEncoding
      else
          withSystemTempFile "csv2xml.xml" $ \tmpFileP tmpFileH -> do
            processFile sourceName inFileContent tmpFileH outputEncoding
            copyFile tmpFileP outFileName
