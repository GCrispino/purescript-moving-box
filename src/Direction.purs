module Direction
    (Direction(..)
    , HorizontalDirection(..)
    , VerticalDirection(..)
    , toHorizontal
    , toVertical
    ) where

import Prelude
import Data.Maybe(Maybe(..))

data HorizontalDirection = LeftDir | RightDir
derive instance eqHDir :: Eq HorizontalDirection

data VerticalDirection   = UpDir | DownDir
derive instance eqVDir :: Eq VerticalDirection

data Direction = Horizontal HorizontalDirection
    | Vertical VerticalDirection

toHorizontal :: Direction -> Maybe HorizontalDirection
toHorizontal (Horizontal a) = Just a 
toHorizontal (Vertical _) = Nothing

toVertical :: Direction -> Maybe VerticalDirection
toVertical (Vertical a) = Just a 
toVertical (Horizontal _) = Nothing

derive instance eqDir :: Eq Direction

instance showHDir :: Show HorizontalDirection where
    show RightDir = "RightDir"
    show LeftDir = "LeftDir"

instance showVDir :: Show VerticalDirection where
    show UpDir = "UpDir"
    show DownDir = "DownDir"
