module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPrint, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let port = (read $ head args :: Int)
--   run show port

--run :: PortID -> IO ()
run :: String -> IO ()
run port = withSocketsDo $ do
  let p = fromIntegral $ (read port :: Int)
  sock <- listenOn $ (PortNumber p)
  putStrLn $ "Listening on " ++ show p
  sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  _ <- forkIO $ commandProcessor handle
  sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
  line <- hGetLine handle
  let cmd = words line
  case (head cmd) of
   ("echo") -> echoCommand handle cmd
   ("add") -> addCommand handle cmd
   _ -> hPutStrLn handle "Unknown command"
  commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
  hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd =
  hPrint handle $ (read $ cmd !! 1 :: Int) + (read $ cmd !! 2 :: Int)
