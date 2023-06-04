module Main where

import System.Environment
import System.IO
import System.Exit
import System.Hardware.Serialport

defaultPort :: FilePath
defaultPort = "/dev/ttyUSB0"

test :: FilePath -> IO ()
test port = hWithSerial port defaultSerialSettings $ \h -> do
 --hPutStr h "AT\r"
 hGetLine h >>= print

main :: IO ()
main = do
 getArgs >>= \case
  []     -> test defaultPort
  [port] -> test port
  _      -> putStrLn "Incorrect number of arguments" >> exitFailure
