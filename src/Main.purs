module Main
  ( main
  ) where

import Prelude

import Data.Array (filter, snoc)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (capture_, targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

todoItem :: { text :: String, onDelete :: Effect Unit } -> React.JSX
todoItem { text: taskString, onDelete: taskAction } =
  R.li
    { className:
        "todo-item flex justify-between p-2 bg-gray-50 mb-2 border rounded"
    , children:
        [ R.span_
            [ R.text taskString ]
        , R.button
            { className:
                "bg-red-500 text-white px-2 py-1 rounded"
            , onClick:
                capture_ taskAction
            , children:
                [ R.text "✕" ]
            }
        ]
    }

mkTodoApp :: Component {}
mkTodoApp = component "TodoApp" \_ -> React.do
  todos /\ setTodos <- useState ([] :: Array String)
  inputText /\ setInputText <- useState ""

  let
    handleInputChange = handler targetValue \mVal ->
      case mVal of
        Just val -> setInputText $ const val
        Nothing -> pure unit

    -- <<< is the usual composition of functions
    addTodo = setTodos <<< flip snoc
    handleAdd = capture_ do
      if inputText == "" then pure unit
      else do
        addTodo inputText
        setInputText $ const ""

    handleDelete taskToDelete =
      setTodos $ filter (_ /= taskToDelete)

  pure $ R.div
    { className:
        "todo-app p-6 bg-white rounded-xl shadow-lg max-w-sm"
    , children:
        [ R.h2
            { className:
                "text-2xl font-bold mb-4 text-gray-800"
            , children:
                [ R.text "Functional Tasks" ]
            }
        , R.div
            { className:
                "input-group flex gap-2 mb-4"
            , children:
                [ R.input
                    { value: inputText
                    , onChange: handleInputChange
                    , placeholder: "Add a task..."
                    , className: "flex-1 p-2 border rounded"
                    }
                , R.button
                    { onClick: handleAdd
                    , className:
                        "bg-indigo-600 text-white px-4 py-2 rounded"
                    , children:
                        [ R.text "Add" ]
                    }
                ]
            }
        , R.ul_ $ todos <#> todoItem <<< \text ->
            { text, onDelete: handleDelete text }
        ]
    }

main :: Effect Unit
main = do
  doc <- document =<< window
  container <- getElementById "app" $ toNonElementParentNode doc

  case container of
    Nothing -> pure unit
    Just el -> do
      app <- mkTodoApp
      root <- createRoot el
      renderRoot root $ app {}