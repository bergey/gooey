{-# LANGUAGE OverloadedStrings #-}

-- | Data types for the TodoMVC demo: state and actions (edges in the
-- state machine).

module Types where

import           Data.JSString
import           GHCJS.Types

data Task = Task
            { description :: JSString
            , completed   :: Bool
            , edits       :: Maybe JSString
            , taskId      :: Int
            } deriving (Show, Eq)

mkTask :: JSString -> Int -> Task
mkTask desc i = Task desc False Nothing i

data State = State
             { tasks      :: [Task]
             , field      :: JSString
             , uid        :: Int
             , visibility :: Visibility
             } deriving (Show, Eq)

data Visibility = Completed | Active | All
                deriving (Show, Eq)

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

type Push a = a -> IO ()

type Pop a = IO a

type TaskAction' = (Int, TaskAction)

initialState :: State
initialState = State [] "" 0 All
