--------------------------------------------------------------------------------
{- |
 - Program     : a2h
 - Description : Converts input ASCII-HEX into a binary output.
 - Copyright   : (c) A.Calhoon, 2017-2018
 - License     : BSD3
 - Maintainer  : a.calhoon@gmail.com
 - Stability   : experimental
 - Portability : POSIX
 - 
 - Converts input sources into binary output to a file or stdout.
 -}
--------------------------------------------------------------------------------
module Main where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class     (liftIO)
import           Data.Char                  (chr)
import           Data.Foldable              (traverse_)
import           Data.Monoid                ((<>))
import           Numeric                    (readHex)
import qualified Options.Applicative        as OA
import           System.Directory           (removeFile)
import           System.IO

--------------------------------------------------------------------------------
-- | Input data source.
data InputSource = Stdin              -- ^ Input coming from stdin.
                 | InFiles [FilePath] -- ^ Input coming from file(s).

--------------------------------------------------------------------------------
-- | Output data sink.
data OutputSink = Stdout           -- ^ Output going to stdout.
                | OutFile FilePath -- ^ Output going to a named file.

--------------------------------------------------------------------------------
-- | Configurable options from the command line.
data Options =
  Options
  { sink   :: OutputSink  -- ^ Where output is going.
  , source :: InputSource -- ^ Where to retrieve input from.
  }

--------------------------------------------------------------------------------
-- | A parser for when one or more input filenames is provided.
inFilesParser :: OA.Parser InputSource
inFilesParser = InFiles <$> (OA.some . OA.strArgument $ OA.metavar "FILES...")

--------------------------------------------------------------------------------
-- | A parser for the stdin flag.
stdinParser :: OA.Parser InputSource
stdinParser =
  OA.flag' Stdin
  (  OA.long "stdin"
  <> OA.help "Read input from stdin"
  )

--------------------------------------------------------------------------------
-- | A parser for when an output filename is provided.
outFileParser :: OA.Parser OutputSink
outFileParser = OutFile <$> OA.strArgument (OA.metavar "FILE")

--------------------------------------------------------------------------------
-- | A parser for the stdout flag.
stdoutParser :: OA.Parser OutputSink
stdoutParser =
  OA.flag' Stdout
  (  OA.long "stdout"
  <> OA.help "Write output to stdout"
  )

--------------------------------------------------------------------------------
-- | A parser for all of the command line args.
optionsParser :: OA.Parser Options
optionsParser =
  Options <$> (outFileParser OA.<|> stdoutParser)
          <*> (inFilesParser OA.<|> stdinParser)

--------------------------------------------------------------------------------
-- | Parse command line options and report usage on failure.
main :: IO ()
main = a2h =<< OA.execParser opts
  where opts = OA.info (OA.helper <*> optionsParser)
                       (OA.fullDesc <> desc <> hdr)
        desc = OA.progDesc " Read ASCII-HEX input and attempt to convert it \
                           \ to binary output."
        hdr  = OA.header "a2h - converts input ASCII-HEX into binary"


a2h :: Options -> IO ()
a2h = undefined

--------------------------------------------------------------------------------
-- | Parse an input ASCII-HEX string
parseHex :: String -> Either String [Int]
parseHex [] = Right []
parseHex (a:b:xs) = case readHex [a,b] of
  [(v,"")] -> (v:) <$> parseHex xs
  _        -> Left "String is invalid."
parseHex _  = Left "Invalid number of ASCII characters"

putBin xs = traverse_ (putChar . chr)

putError err = hPutStrLn stderr err

doConvert = either (hPutStrLn stderr) (traverse_ (putChar . chr))
