-- | Render the TodoMVC demo to VDom.

module Render where

import           Data.Bool
import           GHCJS.VDOM
import           GHCJS.VDOM.Element

render :: State -> VNode
render s =
  div [ class_ "todomvc-wrapper", style "visibility: hidden"]
  [ section (id "todoapp")
    [ taskEntry (field s)
    , taskList (visibility s) (tasks s)
    , controls (visibility s) (tasks s)
    ]
  , infoFooter ]

taskEntry :: Push TaskAction -> String -> VNode
taskEntry raise task =
  header (id "header")
  [ h1 () [text "todos"]
  , input
    [ id "new-todo"
      -- TODO placeholder text
    , autofocus True
    , value task
    , name "newTodo"
      -- TODO use input events for normal text entry
    , keypress (raise . handleKeypress)
    ] ()
  ]

handleKeypress :: KeyboardEvent -> TaskAction
handleKeypress ev = case key ev of
  "Enter" -> Commit
  "Escape" -> Cancel
  k -> Edit k

taskList :: Push Action -> String -> [Task] -> VNode
taskList raise visibility tasks = let
    isVisible t = case visibility of
      "Completed" -> completed t
      "Active" -> not $ completed t
      "All" -> True
    allCompleted = all completed tasks
    cssVisibility = case tasks of
      [] -> "hidden"
      _ -> "visible"
    in
      section [id "main", style ("visibility: " <> cssVisibility)]
        [ input
          [ id "toggle-all"
          , type_ "checkbox"
          , name "toggle"
          , checked allCompleted
          , click . const . raise . CheckAll . not $ allCompleted
          ] []
        , label (for "toggle-all")
            [ text "Mark all as complete"]
        , ul (id "todo-list")
            [ renderTask (raise . updateTask) <$> filter isVisible tasks]
      ]

renderTask :: Push TaskAction -> Task -> VNode
renderTask raise task = let
  className = c ++ ed
  c = if completed task then "completed" else ""
  ed = case edits task of
    Just _ -> "editing"
    Nothing -> ""
  description = fromMaybe (description task) (edits task)
  in
    li (class_ className)
      [ div (class_ "view")
        [ input
          [ class_ "toggle"
          , type_ "checkbox"
          , checked $ completed task
          , click . const $ raise (taskId task, Completed . not . completed $ task)
          ]
        , label (dblclick . const . raise $ (taskId task, Focus))
            [text description]
        , button
            [ class_ "destroy"
            , click . const . raise (taskId task, Delete)
            ]
            []
        ]
      , input
          [ class_ "edit"
          , value description
          , name "title"
          , id ("todo-" <> show (taskId task))
          , keypress $ raise . handleKeypress
          , blur . const . raise $ (taskId task, Commit)
          ]
      ]

controls :: Push Action -> Visibility -> [Task] -> VNode
controls raise vis tasks = let
  tasksCompleted = length $ filter completed tasks
  tasksLeft = length tasks - tasksCompleted
  item_ = if tasksLeft = 1 then " item" else " items"
  in
    footer [id "footer", hidden (tasks == [])]
      [ span (id "todo-count")
        [ strong () [text () (show tasksLeft)]
        , text (item_ <> " left" )
        ]
      , ul (id "filters")
          [ visibilitySwap raise "#/" All vis
          , text " "
          , visibilitySwap raise "#/active" Active vis
          , text " "
          , visibilitySwap raise "#/completed" Completed vis
          ]
      , button
          [ class_ "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , click . const . raise $ DeleteComplete
          ]
          [ text $ "Clear completed (" <> show tasksCompleted <> ")" ]
      ]

visibilitySwap :: Push Action -. String -> Visibility -> Visibility -> VNode
visibilitySwap raise uri vis actualVis = let
  className = if vis == actualVis then "selected" else ""
  in
    li ( click . const . raise $ ChangeVisibility vis )
      [ a [class_ className, href uri] [ text $ show vis ] ]

infoFooter :: VNode
infoFooter =
  footer (id "info")
    [ p () [ text "Double-click to edit a todo"]
    , p () [ text "Written by ", a (href "https://github.com/bergey") [text "Daniel Bergey"]]
    , p () [ text "Part of ", a (href "http://todomvc.com" [text "TodoMVC"])]
    ]
