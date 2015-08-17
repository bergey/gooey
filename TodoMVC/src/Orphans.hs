module Orphans where

import           Data.JSString
import           Data.Semigroup

instance Semigroup JSString where
  (<>) = mappend
