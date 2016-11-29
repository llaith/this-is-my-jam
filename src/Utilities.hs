module Utilities (
  rightToMaybe,
  firstJust
) where

import           Control.Monad (join)
import           Data.List     (find)
import           Data.Maybe    (isJust)


rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just


firstJust :: [Maybe a] -> Maybe a
firstJust = join . find isJust
