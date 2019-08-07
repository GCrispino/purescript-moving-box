module Direction
    (Direction(..)
    , isHorizontal
    , isVertical
    , ifIsHorizontal
    , ifIsVertical
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

ifIsHorizontal :: forall a. Direction -> a -> a -> a
ifIsHorizontal dir x y = if (isHorizontal dir) then x else y

ifIsVertical :: forall a. Direction -> a -> a -> a
ifIsVertical dir x y = if (isVertical dir) then x else y

derive instance eqDir :: Eq Direction
instance showDir :: Show Direction where
    show RightDir = "RightDir"
    show LeftDir = "LeftDir"
    show UpDir = "UpDir"
    show DownDir = "DownDir"

