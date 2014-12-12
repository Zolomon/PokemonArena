module Main where
import System.Environment (getArgs)
import Control.Monad (unless)
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
validate (x:xs) = case x of
                   "server" -> unless (length xs == 1) $ error "Format: port"
                   "client" -> unless (length xs == 2) $ error "Format: hostname port"
                   _ -> error "Unknown command"

runServer :: String -> IO ()
runServer =
