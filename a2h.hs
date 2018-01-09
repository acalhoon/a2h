{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad.IO.Class     (liftIO)
import           Control.Exception          (Exception, throwIO, onException)
import           Control.Monad.Catch        (throwM)
import           Control.Monad              (void, mapM_)
import           Data.Char                  (isSpace, chr)
import           Data.Monoid                ((<>))
import           Numeric                    (readHex)
import qualified Options.Applicative        as OA
import           System.Directory           (removeFile)
import           System.IO
import           System.IO.Temp             (withSystemTempFile)
import           Data.Conduit
import qualified Data.Conduit.Combinators   as CC
import qualified Data.Conduit.List          as CL
import qualified Data.ByteString            as BS
import qualified Data.Word8                 as W8

data ParseError = InvalidData String -- ^ invalid data found and reported
                | InvalidLength      -- ^ data length prevented accurate parsing
instance Exception ParseError

--------------------------------------------------------------------------------
-- | Common ParseError header message.
parseExcHeader :: String
parseExcHeader = "Parse Error: "

--------------------------------------------------------------------------------
-- | Allow ParseError exceptions to show details on failure.
instance Show ParseError where
  show (InvalidData s) = parseExcHeader ++ "input contains invalid hex -- " ++ s
  show InvalidLength = parseExcHeader ++ "ran out of input"

w8ToString :: [W8.Word8] -> String
w8ToString = map (chr . fromIntegral)

loop :: ConduitM BS.ByteString c IO ()
loop = do bs <- CC.takeE 2 .| CC.fold
          case BS.length bs of
            2 -> case readHex . w8ToString . BS.unpack $ bs of
                   [(v,"")] -> do liftIO . hPutChar stderr . chr $ v
                                  loop
                   _ -> throwM $ InvalidData $ show bs
            0 -> return ()
            _ -> throwM InvalidLength

main = runConduit $ do { yield "4144414d"; yield "20"; yield "43414c484f4f4e" } .| loop

-- xacktmf s | Data.Text.null s = Nothing
--           | otherwise = fromPathPiece s

--  --------------------------------------------------------------------------------
--  -- | Input data source.
--  data InputSource = Stdin              -- ^ Input coming from stdin.
--                   | InFiles [FilePath] -- ^ Input coming from file(s).
--  
--  --------------------------------------------------------------------------------
--  -- | Output data sink.
--  data OutputSink = Stdout           -- ^ Output going to stdout.
--                  | OutFile FilePath -- ^ Output going to a named file.
--  
--  --------------------------------------------------------------------------------
--  -- | Configurable options from the command line.
--  data Options =
--    Options
--    { sink   :: OutputSink  -- ^ Where output is going.
--    , source :: InputSource -- ^ Where to retrieve input from.
--    }
--  
--  --------------------------------------------------------------------------------
--  -- | A parser for when one or more input filenames is provided.
--  inFilesParser :: OA.Parser InputSource
--  inFilesParser = InFiles <$> (OA.some . OA.strArgument $ OA.metavar "FILES...")
--  
--  --------------------------------------------------------------------------------
--  -- | A parser for the stdin flag.
--  stdinParser :: OA.Parser InputSource
--  stdinParser =
--    OA.flag' Stdin
--    (  OA.long "stdin"
--    <> OA.help "Read input from stdin"
--    )
--  
--  --------------------------------------------------------------------------------
--  -- | A parser for when an output filename is provided.
--  outFileParser :: OA.Parser OutputSink
--  outFileParser =
--   OutFile <$> OA.strOption (  OA.long "out"
--                            <> OA.short 'o'
--                            <> OA.help "Output file name"
--                            <> OA.metavar "TARGET"
--                            )
--  
--  --------------------------------------------------------------------------------
--  -- | A parser for the stdout flag.
--  stdoutParser :: OA.Parser OutputSink
--  stdoutParser =
--    OA.flag' Stdout
--    (  OA.long "stdout"
--    <> OA.help "Write output to stdout"
--    )
--  
--  --------------------------------------------------------------------------------
--  -- | A parser for all of the command line args.
--  optionsParser :: OA.Parser Options
--  optionsParser =
--    Options <$> (outFileParser OA.<|> stdoutParser)
--            <*> (inFilesParser OA.<|> stdinParser)
--  
--  --------------------------------------------------------------------------------
--  -- | Parse command line options and report usage on failure.
--  main :: IO ()
--  main = a2h =<< OA.execParser opts
--    where opts = OA.info (OA.helper <*> optionsParser)
--                         (OA.fullDesc <> desc <> hdr)
--          desc = OA.progDesc " Read ASCII-HEX input and attempt to convert it \
--                             \ to binary output."
--          hdr  = OA.header "a2h - converts input ASCII-HEX into binary"
--  
--  --------------------------------------------------------------------------------
--  -- | Turn command line arguments @opt@ into output.
--  a2h :: Options -> IO ()
--  a2h opt = runA2H (source opt) (sink opt)
--  
--------------------------------------------------------------------------------
-- | Parsing exceptions
-- data ParseError = InvalidData String -- ^ invalid data found and reported
--                 | InvalidLength      -- ^ data length prevented accurate parsing
-- instance Exception ParseError
-- 
-- --------------------------------------------------------------------------------
-- -- | Common ParseError header message.
-- parseExcHeader :: String
-- parseExcHeader = "Parse Error: "
-- 
-- --------------------------------------------------------------------------------
-- -- | Allow ParseError exceptions to show details on failure.
-- instance Show ParseError where
--   show (InvalidData s) = parseExcHeader ++ "input contains invalid hex -- " ++ s
--   show InvalidLength = parseExcHeader ++ "ran out of input"
-- 
--  --------------------------------------------------------------------------------
--  -- | Parse an ASCII-HEX string writing its contents converted contents to
--  -- @outfile@.
--  parseHex :: String -> Handle -> IO ()
--  parseHex [] _ = return () 
--  parseHex (a:b:xs) outfile = case readHex [a,b] of
--    [(v,"")] -> (hPutChar outfile $ chr v) >> parseHex xs outfile
--    _        ->  throwIO $ InvalidData (show [a,b])
--  parseHex _ _ = throwIO $ InvalidLength
--  
--  --------------------------------------------------------------------------------
--  -- | Read input from @infile@ and write its parsed results to @outfile@.
--  parseHandleTo :: Handle -> Handle -> IO ()
--  parseHandleTo infile outfile =
--    hGetContents infile >>= flip parseHex outfile . filter (not . isSpace)
--  
--  --------------------------------------------------------------------------------
--  -- | Read input from the file at @inpath@ and write its parsed results to
--  -- @outfile@.
--  parseFileTo :: FilePath -> Handle -> IO ()
--  parseFileTo inpath outfile = withFile inpath ReadMode $ flip parseHandleTo outfile
--  
--  --------------------------------------------------------------------------------
--  -- | Reads input from the specified source(s) and writes the successful output
--  -- to @outfile@.
--  runParse :: InputSource -> Handle -> IO ()
--  runParse Stdin outfile = parseHandleTo stdin outfile
--  runParse (InFiles fs) outfile = mapM_ (flip parseFileTo outfile) fs
--  
--  --------------------------------------------------------------------------------
--  -- | Parse from input to output.
--  runA2H :: InputSource -> OutputSink -> IO ()
--  runA2H src Stdout =
--    withSystemTempFile "a2h.bin" $ \_ h -> do
--      runParse src h
--      hSeek h AbsoluteSeek 0
--      s <- hGetContents h
--      putStr s
--  runA2H src (OutFile path) = mkFile path `onException` rmFile path
--    where mkFile n = withFile n WriteMode $ runParse src
--          rmFile = void . removeFile
