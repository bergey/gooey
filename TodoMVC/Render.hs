{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Render the TodoMVC demo to VDom.

module Render where

import           Orphans
import           Types

import           Data.Bool
import           GHCJS.Types
import           GHCJS.VDOM
import           GHCJS.VDOM.Attribute as A
import           GHCJS.VDOM.Element   as E
import           GHCJS.VDOM.Event     as V

import           Data.Maybe
import           Data.Semigroup       (Semigroup (..))
import           GHC.Exts             (IsString, fromString)

import           Prelude              hiding (div)

render :: Push Action -> State -> VNode
render raise s =
  div [ class_ "todomvc-wrapper", style "visibility: hidden"]
  [ section (A.id "todoapp")
    [ taskEntry raise (field s)
    , taskList raise (visibility s) (tasks s)
    , controls raise (visibility s) (tasks s)
    ]
  , infoFooter ]

taskEntry :: Push Action -> JSString -> VNode
taskEntry raise task =
  header (A.id "header")
  [ h1 () [text "todos"]
  , input
    [ A.id "new-todo"
      -- TODO placeholder text
    , autofocus True
    , value task
    , name "newTodo"
      -- TODO use input events for normal text entry
    , keypress (raise . entryHandler)
    ] ()
  ]

entryHandler :: KeyboardEvent -> Action
entryHandler ev = case V.key ev of
  "Enter" -> Add
  "Escape" -> NoOp
  k -> UpdateField k

taskHandler :: KeyboardEvent -> TaskAction
taskHandler ev = case V.key ev of
  "Enter" -> Commit
  "Escape" -> Cancel
  k -> Edit k

taskList :: Push Action -> Visibility -> [Task] -> VNode
taskList raise visibility tasks = let
    isVisible t = case visibility of
      Completed -> completed t
      Active -> not $ completed t
      All -> True
    allCompleted = all completed tasks
    cssVisibility = case tasks of
      [] -> "hidden"
      _ -> "visible"
    in
      section [A.id "main", style ("visibility: " <> cssVisibility)]
        [ input
          [ A.id "toggle-all"
          , type_ "checkbox"
          , name "toggle"
          , checked allCompleted
          , click . const . raise . CheckAll . not $ allCompleted
          ] ()
        , label (for "toggle-all")
            [ text "Mark all as complete"]
        , ul (A.id "todo-list")
            ( renderTask (raise . uncurry UpdateTask) <$> filter isVisible tasks )
      ]

renderTask :: Push TaskAction' -> Task -> VNode
renderTask raise task = let
  className = c <> ed :: JSString
  c = if completed task then "completed" else ""
  ed = case edits task of
    Just _ -> "editing"
    Nothing -> ""
  desc = fromMaybe (description task) (edits task)
  in
    li (class_ className)
      [ div (class_ "view")
        [ input
          [ class_ "toggle"
          , type_ "checkbox"
          , checked $ completed task
          , click . const $ raise (taskId task, Complete . not . completed $ task)
          ]
          ()
        , label (dblclick . const . raise $ (taskId task, Focus))
            [text desc]
        , E.button
            [ class_ "destroy"
            , click . const . raise $ (taskId task, Delete)
            ]
            ()
        ]
      , input
          [ class_ "edit"
          , value desc
          , name "title"
          , A.id ("todo-" <> show' (taskId task))
          , keypress $ raise . (taskId task,) . taskHandler
          , blur . const . raise $ (taskId task, Commit)
          ]
          ()
      ]

controls :: Push Action -> Visibility -> [Task] -> VNode
controls raise vis tasks = let
  tasksCompleted = length $ filter completed tasks
  tasksLeft = length tasks - tasksCompleted
  item_ = if tasksLeft == 1 then " item" else " items"
  in
    footer [ A.id "footer", hidden (tasks == []) ]
      [ E.span (A.id "todo-count")
        [ strong () [ text $ show' tasksLeft ]
        , text (item_ <> " left" )
        ]
      , ul (A.id "filters")
          [ visibilitySwap raise "#/" All vis
          , text " "
          , visibilitySwap raise "#/active" Active vis
          , text " "
          , visibilitySwap raise "#/completed" Completed vis
          ]
      , E.button
          [ class_ "clear-completed"
          , A.id "clear-completed"
          , hidden (tasksCompleted == 0)
          , click . const . raise $ DeleteComplete
          ]
          [ text $ "Clear completed (" <> show' tasksCompleted <> ")" ]
      ]

visibilitySwap :: Push Action -> JSString -> Visibility -> Visibility -> VNode
visibilitySwap raise uri vis actualVis = let
  className = if vis == actualVis then "selected" else ""
  in
    li ( click . const . raise $ ChangeVisibility vis )
      [ a [class_ className, href uri] [ text $ show' vis ] ]

infoFooter :: VNode
infoFooter =
  footer (A.id "info")
    [ p () [ text "Double-click to edit a todo"]
    , p () [ text "Written by ", a (href "https://github.com/bergey") [text "Daniel Bergey"]]
    , p () [ text "Part of ", a (href "http://todomvc.com") [text "TodoMVC"]]
    ]

show' :: (Show a, IsString s) => a -> s
show' = fromString . show
