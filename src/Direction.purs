module Direction
    (Direction(..)
    , isHorizontal
    , isVertical
    ) where

import Prelude

data Direction = LeftDir | RightDir | UpDir | DownDir

isHorizontal :: Direction -> Boolean
isHorizontal RightDir = true
isHorizontal LeftDir = true
isHorizontal _ = false

isVertical :: Direction -> Boolean
isVertical UpDir = true
isVertical DownDir = true
isVertical _ = false

derive instance eqDir :: Eq Direction
instance showDir :: Show Direction where
    show RightDir = "RightDir"
    show LeftDir = "LeftDir"
    show UpDir = "UpDir"
    show DownDir = "DownDir"

