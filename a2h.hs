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

import           Control.Exception          (IOException, try)
import           Control.Monad
-- import           Control.Monad              ((<$!>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
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
a2h = undefined -- opt = writeOutput (sink opt) (parseInput $ source opt)

--------------------------------------------------------------------------------
-- | Parsing computation within IO.
type Error = String
type ParseResult = Either Error ()

--------------------------------------------------------------------------------
-- | Parse an input ASCII-HEX string
parseHex :: Handle -> String -> ExceptT Error IO ()
parseHex _ [] = return () 
parseHex h (a:b:xs) = case readHex [a,b] of
  [(v,"")] -> liftIO (hPutChar h $ chr v) >> parseHex h xs
  _        -> throwE $ "String " ++ show [a,b] ++ " is invalid."
parseHex _ _ =  throwE $ "Invalid number of ASCII characters"

--------------------------------------------------------------------------------
-- | Write successful parsed @res@ to the handle @outfile@.
runParseResult :: IO a -> IO a -> ParseResult -> IO a
runParseResult doFail doSuccess = either (\err -> reportFailure err >> doFail) (\_ -> doSuccess)
  where reportFailure err = hPutStr stderr "Parse error: " >> hPutStrLn stderr err

--------------------------------------------------------------------------------
-- | Reads input from the specified source(s) and attempts to parse it.
-- parseInput :: InputSource -> Handle -> IO ParseResult
parseInput :: InputSource -> Handle -> IO ParseResult
parseInput input outfile = case input of
  Stdin        -> runExceptT $ liftIO getContents >>= parse
  (InFiles fs) -> runExceptT . mapM_ parseFile $ fs
  where parse = parseHex outfile . filter (not . isSpace)
        parseHandle infile = parse <$> (liftIO $ hGetContents infile)
        parseFile name = liftIO $ withFile name ReadMode (runExceptT . parseHandle)

--------------------------------------------------------------------------------
-- | Writes successfully parsed input to the output sink.
writeOutput :: OutputSink -> InputSource -> IO ()
writeOutput Stdout infiles =
  withSystemTempFile "a2h.bin" $ \_ h -> do
    e <- parseInput infiles h
    runParseResult (return ()) (dumpTemp h) e
  where dumpTemp h = do { hSeek h AbsoluteSeek 0; s <- hGetContents h; putStr s }

writeOutput (OutFile f) infiles =  do
  success <- withFile f WriteMode $ \h -> do
    e <- parseInput infiles h
    runParseResult (return False) (return True) e
  unless success $ tryDelete f

tryIOE :: IO a -> IO (Either IOException a)
tryIOE = try

tryDelete :: String -> IO ()
tryDelete name = void $ tryIOE $ removeFile name
