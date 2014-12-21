module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket, HostName, PortNumber)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hPrint, BufferMode(..), Handle)
import Control.Concurrent --(forkIO, newEmptyMVar, MVar)
import qualified Data.Map as Map

import DataTypes

run :: String -> MVar Connections -> IO ()
run port connections = withSocketsDo $ do
  let p = fromIntegral $ (read port :: Int)
  sock <- listenOn $ (PortNumber p)
  putStrLn $ "Listening on " ++ show p
  sockHandler sock connections

sockHandler :: Socket -> MVar Connections -> IO ()
sockHandler sock connections = do
  (handle, host, port) <- accept sock
  hSetBuffering handle NoBuffering
  _ <- forkIO $ do
    conns <- takeMVar connections -- Add new connection
    putMVar connections (Map.insert (host, port) (PlayerState Login "", Account "" "" "", handle) conns)
    commandProcessor handle (host, port) connections
  sockHandler sock connections

commandProcessor :: Handle -> (HostName, PortNumber) -> MVar Connections -> IO ()
commandProcessor handle con connections = do
  line <- hGetLine handle
  let cmd = words line
  conns <- takeMVar connections
  let updatedState = updateLastCommand conns con (head cmd)
  putMVar connections updatedState
  putStrLn (show updatedState)
  case (head cmd) of
   ("echo") -> echoCommand handle cmd
   ("add") -> addCommand handle cmd
   _ -> hPutStrLn handle "Unknown command"
  commandProcessor handle con connections

updateLastCommand :: Connections -> Connection -> String -> Connections
updateLastCommand conns con cmd =
  Map.adjust (\(playerstate, a, h) -> (PlayerState { currentRoom=(currentRoom playerstate)
                                                           , lastCommand=cmd},
                                               a,
                                               h)) con conns

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
  hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd =
  hPrint handle $ (read $ cmd !! 1 :: Int) + (read $ cmd !! 2 :: Int)
