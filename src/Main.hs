module Main where
import System.Environment (getArgs)
--import Control.Monad(unless)
import Server (run)

main :: IO ()
main = do
  args <- getArgs
  validate args
  case (head args) of
   "server" -> runServer (args !! 1)
   --"client" -> runClient (args !! 1)
   _ -> return ()

validate :: [String] -> IO ()
validate [] = error "Not enough args, see --help"
validate (x:xs)
  | x=="server" && length xs /= 1 = error "Format: server <port>"
  | x=="client" && length xs /= 2 = error "Format: client <hostname> <port>"
  | otherwise = return ()


runServer :: String -> IO ()
runServer = Server.run
