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

import           Data.Char                  (isSpace, chr)
import           Data.Monoid                ((<>))
import           Numeric                    (readHex)
import qualified Options.Applicative        as OA
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
outFileParser =
 OutFile <$> OA.strOption (  OA.long "out"
                          <> OA.short 'o'
                          <> OA.help "Output file name"
                          <> OA.metavar "TARGET"
                          )

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

--------------------------------------------------------------------------------
-- | Turn command line arguments @opt@ into output.
a2h :: Options -> IO ()
a2h opt = writeOutput (sink opt) (parseInput $ source opt)

--------------------------------------------------------------------------------
-- | Parsing computation within IO.
type Error = String

--------------------------------------------------------------------------------
-- | Parse an input ASCII-HEX string
parseHex :: String -> Either Error String
parseHex [] = return [] 
parseHex (a:b:xs) = case readHex [a,b] of
  [(v,"")] -> (chr v:) <$> parseHex xs
  _        -> Left $ "String \"" ++ show [a,b] ++ "\" is invalid."
parseHex _ =  Left $ "Invalid number of ASCII characters"

--------------------------------------------------------------------------------
-- | Write successful parsed @res@ to the handle @outfile@.
putParseResult :: Handle -> IO (Either Error String) -> IO ()
putParseResult outfile res = res >>= either (hPutStrLn stderr) (hPutStr outfile)

--------------------------------------------------------------------------------
-- | Reads input from the specified source(s) and attempts to parse it.
parseInput :: InputSource -> IO (Either Error String)
parseInput input = case input of
  Stdin        -> parse <$> getContents
  (InFiles fs) -> parse . concat <$> mapM readFile fs
  where parse = parseHex . filter (not . isSpace)

--------------------------------------------------------------------------------
-- | Writes successfully parsed input to the output sink.
writeOutput :: OutputSink -> IO (Either Error String) -> IO ()
writeOutput Stdout      parse = putParseResult stdout parse
writeOutput (OutFile f) parse = withFile f WriteMode $ \outfile -> putParseResult outfile parse
