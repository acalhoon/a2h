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

import           Control.Monad              
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

a2h opt = do
  me <- runExceptT $ readInput (source opt)
  either (hPutStrLn stderr) (writeOutput (sink opt)) me

--------------------------------------------------------------------------------
-- | Parse an input ASCII-HEX string
parseHex :: String -> ExceptT String IO [Int]
parseHex [] = return [] 
parseHex (a:b:xs) = case readHex [a,b] of
  [(v,"")] -> (v:) <$> parseHex xs
  _        -> throwE $ "String \"" ++ show [a,b] ++ "\" is invalid."
parseHex _ =  throwE $ "Invalid number of ASCII characters"

putBinary :: Handle -> [Int] -> IO ()
putBinary outfile = traverse_ (hPutChar outfile . chr)

parseFile :: FilePath -> ExceptT String IO [Int]
parseFile inpath = do
 ExceptT $ withFile inpath ReadMode $ \infile ->
           hGetContents infile >>= \s ->
           runExceptT $ parseHex s

readInput :: InputSource -> ExceptT String IO [Int]
readInput Stdin        = liftIO (hGetContents stdin) >>= parseHex
readInput (InFiles fs) = foldM (\acc name -> (acc ++) <$> parseFile name) [] fs

writeOutput :: OutputSink -> [Int] -> IO ()
writeOutput Stdout      xs = putBinary stdout xs
writeOutput (OutFile f) xs = withFile f WriteMode $ flip putBinary xs

