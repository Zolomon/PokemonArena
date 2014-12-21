module DataTypes where

import System.IO
import Network (HostName, PortNumber)
import qualified Data.Map as M

data Account = Account { username :: String
                       , password :: String
                       , email :: String
                       } deriving (Eq, Show, Read, Ord)

data RoomState = Login | Town | Wilderness | Arena | Logout
               deriving (Eq, Show, Read, Bounded, Enum, Ord)

data PlayerState = PlayerState { currentRoom :: RoomState
                               , lastCommand :: String
                               } deriving (Eq, Show, Read, Ord)

type Connection = (HostName, PortNumber)
type Player = (PlayerState, Account, Handle)
type Connections = M.Map Connection Player
