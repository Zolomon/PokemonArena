module Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket{-, HostName, PortNumber-})
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
  let connection = (host, port)
  _ <- forkIO $ do
    conns <- takeMVar connections -- Add new connection
    putMVar connections (Map.insert (host, port) (PlayerState Login "", Account "" "" "", handle) conns)
    handleMainMenu handle connection connections
    commandProcessor handle (host, port) connections
  sockHandler sock connections


-- menuState = [(Login, [("New account", LoginNewAccount),
--                       ("Login", LoginLogin),
--                       ("Quit", Logout)]),
--              (LoginNewAccount, [("Enter your username", LoginNewAccountEmail),
--                                 ("Back", Login)]),
--              (LoginNewAccountEmail, [("Enter your email", LoginNewAccountPassword),
--                                      ("Back", LoginNewAccountEmail),
--                                      ("Cancel", Login)]),
--              (LoginNewAccountPassword, [("Enter your password", LoginNewAccountPasswordVerify),
--                                         ("Back", LoginNewAccountPassword),
--                                         ("Cancel", Login)]),
--              (LoginNewAccountPasswordVerify, [("Enter your password again", LoginNewAccountComplete),
--                                               ("Back", LoginNewAccountPassword),
--                                               ("Cancel", Login)])
--             ]


printMenu :: Handle -> Connection -> MVar Connections -> IO ()
printMenu h _ _ = do
  hPutStrLn h "Welcome to Pokemon Arena!"

router :: [(RoomState, Handle -> Connection -> MVar Connections -> IO ())]
router = [(Login, handleMainMenu),
          (LoginNewAccount, handleNewAccount)
         --,(LoginLogin, handleLogin)
         ]


handleMainMenu :: Handle -> Connection -> MVar Connections -> IO ()
handleMainMenu h c cs = do
  let menuItems = [("New account", LoginNewAccount)
                  ,("Login", LoginLogin)
                  ,("Quit", Logout)]
  localLoop h c cs menuItems
  where localLoop :: Handle -> Connection -> MVar Connections -> [(String, RoomState)] -> IO ()
        localLoop _ _ _  menu = do
          _ <- mapM (\(i, x) -> hPutStrLn h (((show (i :: Int)) :: String) ++ ". " ++ (show x))) (zip [0..] menu)
          line <- hGetLine h
          let cmd = words line
          let choice = lookup (read (head cmd) :: Int) (zip [0..] menu)
          case choice of
           Just (_, j) -> case (lookup j router) of
                      Just y -> y h c cs
                      Nothing -> hPutStrLn h "What? Please try again!" >> localLoop h c cs menu
           Nothing -> hPutStrLn h "What? Please try again!" >> localLoop h c cs menu



handleNewAccount :: Handle -> Connection -> MVar Connections -> IO ()
handleNewAccount h c cs = do
  let menuItems = [("Cancel", Login)]
  localLoop h c cs menuItems
      where localLoop :: Handle -> Connection -> MVar Connections -> [(String, RoomState)] -> IO ()
            localLoop _ _ _ menu = do
              _ <- mapM (\(i, x) -> hPutStrLn h ((show (i :: Int)) ++ ". " ++ (show x))) (zip [0..] menu)
              handleUsername

            handleUsername = do
              hPutStrLn h "What is your username? "
              line <- hGetLine h
              verifyUsernameAndHandleEmail $ words line

            verifyUsernameAndHandleEmail :: [String] -> IO ()
            verifyUsernameAndHandleEmail input
              | length input == 1 = do
                  hPutStrLn h "Enter your email:"
                  l <- hGetLine h
                  verifyEmailAndHandlePassword (words l) input
              | otherwise = do
                  hPutStrLn h "Sorry, only a one word names please. Try again."
                  line <- hGetLine h
                  verifyUsernameAndHandleEmail (words line)

            verifyEmailAndHandlePassword :: [String] -> [String] -> IO ()
            verifyEmailAndHandlePassword line input
              | length line == 1 = do
                  hPutStrLn h "Enter your password please: "
                  l <- hGetLine h
                  verifyPasswordAndHandlePasswordRepeat l (head line:input)
              | otherwise = do
                  hPutStrLn h "Sorry, a valid email contains no spaces. Try again."
                  l <- hGetLine h
                  verifyEmailAndHandlePassword (words l) input

            verifyPasswordAndHandlePasswordRepeat :: String -> [String] -> IO ()
            verifyPasswordAndHandlePasswordRepeat pass input = do
              hPutStrLn h "Okay. Next password please."
              nextPass <- hGetLine h
              verifyPasswordRepeatAndLogin pass nextPass input

            verifyPasswordRepeatAndLogin :: String -> String -> [String] -> IO ()
            verifyPasswordRepeatAndLogin pass1 pass2 input
              | pass1 == pass2 = do
                  hPutStrLn h "Account created."
                  let inp = reverse input
                  conns <- takeMVar cs
                  let insertPlayer = Map.insert c (PlayerState {currentRoom = Town, lastCommand=""},
                                                                  Account { username = (inp !! 0)
                                                                          , password = pass1
                                                                          , email = (inp !! 1)
                                                                          },
                                                                  h) conns
                  putMVar cs insertPlayer
                  putStrLn $ "Player created: " ++ (show insertPlayer)
                  commandProcessor h c cs
              | otherwise = do
                  hPutStrLn h "Passwords didn't match. Please re-enter both passwords."
                  verifyEmailAndHandlePassword (drop 1 input) (tail input)


commandProcessor :: Handle -> Connection -> MVar Connections -> IO ()
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
