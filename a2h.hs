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

import           Control.Exception          (Exception, IOException, try, throwIO)
import           Control.Monad              (void)
import           Data.Char                  (isSpace, chr)
import           Data.Monoid                ((<>))
import           Numeric                    (readHex)
import qualified Options.Applicative        as OA
import           System.Directory           (removeFile)
import           System.IO
import           System.IO.Temp

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
a2h opt = writeOutput (sink opt) (source opt)

--------------------------------------------------------------------------------
-- | Parsing computation within IO.
data ParseError = InvalidData String
                | InvalidLength
instance Exception ParseError

instance Show ParseError where
  show (InvalidData s) = "Parse Error: input contains invalid hex -- " ++ s
  show InvalidLength = "Parse Error: ran out of input"

--------------------------------------------------------------------------------
-- | Parse ASCII-HEX string and write its contents converted contents to @h@.
parseHex :: Handle -> String -> IO ()
parseHex _ [] = return () 
parseHex h (a:b:xs) = case readHex [a,b] of
  [(v,"")] -> (hPutChar h $ chr v) >> parseHex h xs
  _        ->  throwIO $ InvalidData (show [a,b])
parseHex _ _ = throwIO $ InvalidLength

--------------------------------------------------------------------------------
-- | Handle parsing failure/success.
runParseResult :: IO a -> IO a -> Either ParseError () -> IO a
runParseResult doFail doSuccess = either (\exc -> reportExc exc >> doFail) (\_ -> doSuccess)
  where reportExc = hPutStrLn stderr . show

--------------------------------------------------------------------------------
-- | Reads input from the specified source(s) and attempts to parse it while
-- writing output to @outfile@.
parseInput :: InputSource -> Handle -> IO ()
parseInput input outfile = case input of
  Stdin        -> getContents >>= parse
  (InFiles fs) -> mapM_ parseFile fs
  where parse = parseHex outfile . filter (not . isSpace)
        parseHandle infile = hGetContents infile >>= parse
        parseFile name = withFile name ReadMode parseHandle

--------------------------------------------------------------------------------
-- | Writes successfully parsed input to the output sink.
writeOutput :: OutputSink -> InputSource -> IO ()
writeOutput Stdout infiles =
  withSystemTempFile "a2h.bin" $ \_ h -> do
    e <- try $ parseInput infiles h
    runParseResult (return ()) (dumpTemp h) e
  where dumpTemp h = do { hSeek h AbsoluteSeek 0; s <- hGetContents h; putStr s }
writeOutput (OutFile f) infiles =  do
  e <- try $ withFile f WriteMode $ \h -> do
    parseInput infiles h 
  runParseResult (tryDelete f) (return ()) e
  where tryDelete = void . tryIOE . removeFile

tryIOE :: IO a -> IO (Either IOException a)
tryIOE = try
