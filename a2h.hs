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

import           Control.Monad              (foldM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import           Data.Char                  (isSpace, chr)
import           Data.Foldable              (traverse_)
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
a2h opt = writeOutput (sink opt) (readInput $ source opt)

--------------------------------------------------------------------------------
-- | Parsing computation within IO.
type IOParse = ExceptT String IO 

--------------------------------------------------------------------------------
-- | Parse an input ASCII-HEX string
parseHex :: String -> IOParse String
parseHex [] = return [] 
parseHex (a:b:xs) = case readHex [a,b] of
  [(v,"")] -> ((chr v):) <$> parseHex xs
  _        -> throwE $ "String \"" ++ show [a,b] ++ "\" is invalid."
parseHex _ =  throwE $ "Invalid number of ASCII characters"

--------------------------------------------------------------------------------
-- | Write successful parsed @res@ to the handle @outfile@.
putBinary :: Handle -> IOParse String -> IO ()
putBinary outfile res = runExceptT res >>= either reportError putOutput
   where reportError = hPutStrLn stderr
         putOutput = traverse_ (liftIO . hPutChar outfile)

--------------------------------------------------------------------------------
-- | Parse the contents of a file from handle @infile@.
parseHandle :: Handle -> IOParse String
parseHandle infile = ExceptT $ hGetContents infile >>= runExceptT . parseHex . filter (not . isSpace)

--------------------------------------------------------------------------------
-- | Parse the contents of a file located at @inpath@.
parseFile :: FilePath -> IOParse String
parseFile inpath = ExceptT $ withFile inpath ReadMode $ runExceptT . parseHandle

--------------------------------------------------------------------------------
-- | Reads input from the specified source(s) and attempts to parse it.
readInput :: InputSource -> IOParse String
readInput Stdin        = parseHandle stdin
readInput (InFiles fs) = foldM (\acc name -> (acc ++) <$> parseFile name) [] fs

--------------------------------------------------------------------------------
-- | Writes successfully parsed input to the output sink.
writeOutput :: OutputSink -> IOParse String -> IO ()
writeOutput Stdout      parse = putBinary stdout parse
writeOutput (OutFile f) parse = withFile f WriteMode $ \outfile -> putBinary outfile parse
