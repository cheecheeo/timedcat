{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified System.IO as IO

import qualified Pipes as P
import Pipes ((>->), MonadIO, Consumer', Producer')
import qualified Pipes.Prelude as PP
import System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CmdArgs

data Args = Args { ofilename :: String
                 , seconds :: Int
                 } deriving (Show, Data, Typeable)

argsSpec :: Args
argsSpec = Args {
          ofilename =    CmdArgs.def
                      &= CmdArgs.typFile
                      &= CmdArgs.help "write to FILE",
          seconds   =    CmdArgs.def
                      &= CmdArgs.typ "SECONDS"
                      &= CmdArgs.help "number of seconds to wait before consuming input"
         } &= CmdArgs.summary "Output STDIN to a file after every N seconds of no output."

-- TODO catch eof somewhere in here
racedGetLines :: Int -> IO [String]
racedGetLines n = go []
  where go acc = do
          input <- IO.hWaitForInput IO.stdin n
          if input
            then do
              line <- getLine -- check if EOF here and return if so
              go (acc ++ [line])
            else
              return acc

chunkedLines :: Int -> Producer' [String] IO r
chunkedLines n = PP.repeatM (racedGetLines n) >-> PP.filter (not . null)

pPwriteFile :: (MonadIO m) => FilePath -> Consumer' String m r
pPwriteFile f = P.for P.cat (\txt -> P.liftIO $ IO.withFile f IO.WriteMode (\hdl -> IO.hPutStr hdl txt))

writeChunkedLines :: Int -> String -> IO ()
writeChunkedLines n filename = P.runEffect $ chunkedLines (n * 1000) >-> PP.map unlines >-> writePipe
  where writePipe =
          case filename of
            "" -> error "Need an output filename" -- temporary file
            "-" -> chomp >-> PP.stdoutLn
            fname -> pPwriteFile fname
        chomp = PP.map init

main :: IO ()
main = do
  args <- CmdArgs.cmdArgs argsSpec
  writeChunkedLines (seconds args) (ofilename args)
