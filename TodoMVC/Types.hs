-- | Data types for the TodoMVC demo: state and actions (edges in the
-- state machine).

import GHCJS.Prim

module Types where

data Task = Task
            { description :: String
            , completed   :: Bool
            , edits       :: Maybe String
            , taskId          :: Int
            }

data State = State
             { tasks      :: [Task]
             , field      :: String
             , uid        :: Int
             , visibility :: Visibility
             }

data Visibility = Completed | Active | All
                deriving Show

data TaskAction
  = Focus
  | Edit JSString
  | Cancel
  | Commit
  | Complete Bool
  | Delete

data Action
  = NoOp
  | UpdateField JSString
  | Add
  | UpdateTask Int TaskAction
  | DeleteComplete
  | CheckAll Bool
  | ChangeVisibility Visibility

updateTask :: TaskAction -> Action
updateTask (i, a) = UpdateTask i a

type Push a = a -> IO ()

type Pop a = IO a

initialState :: State
initialState = State [] "" 0 All
