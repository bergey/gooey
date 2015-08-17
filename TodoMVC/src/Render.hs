{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Render the TodoMVC demo to VDom.

module Render where

import           Data.Bool
import           GHCJS.Types
import           GHCJS.VDOM
import           GHCJS.VDOM.Attribute as A
import           GHCJS.VDOM.Element   as E
import           GHCJS.VDOM.Event     as V hiding (input, value)

import           Data.Maybe
import           Data.Semigroup       (Semigroup (..))
import           GHC.Exts             (IsString, fromString)

import           Prelude              hiding (div)

render :: (() -> IO ()) -> () -> VNode
render raise s = input
    [ A.id "new-todo"
    , autofocus True
    -- , value (field s)
    , name "newTodo"
    , keypress (raise . entryHandler)
    ] ()

entryHandler :: KeyboardEvent -> ()
entryHandler ev = ()
