module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple
import Effect (Effect)
import Effect.Timer (setTimeout)

import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body, toDocument)
import Web.HTML.HTMLElement (HTMLElement, toNode)
import Web.HTML.Window (
    document,
    cancelAnimationFrame,
    innerWidth,
    innerHeight,
    requestAnimationFrame
)
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild, setTextContent)

data Direction = HorizontalDirection | VerticalDirection
data HorizontalDirection = LeftDir | RightDir
data VerticalDirection = UpDir | DownDir

createElementWithContent :: String -> String -> HTMLDocument -> Effect Element.Element 
createElementWithContent tag content d = do 
      el <- createElement tag (toDocument d)
      setTextContent content (Element.toNode el)
      pure el

-- foreign import setClassName :: String -> Element.Element -> Effect Unit
foreign import setStyleProp :: String -> String -> Element.Element -> Effect Boolean

createBoxElement :: String -> Document -> Effect Element.Element
createBoxElement id document = do
    boxEl <- createElement "div" document
    Element.setId id boxEl
    Element.setClassName "box" boxEl
    pure boxEl

getBodyNodeFromMaybe :: HTMLDocument -> Node -> Maybe HTMLElement -> Node
getBodyNodeFromMaybe d defaultNode mB = case mB of 
    Nothing -> defaultNode
    Just b -> toNode b

getNewHDirectionAndDist :: HorizontalDirection -> Number -> Int -> Tuple HorizontalDirection Number
getNewHDirectionAndDist RightDir distValPx width = if distValPx >= (toNumber width) - 100.0
                            then Tuple LeftDir distValPx
                            else Tuple RightDir (distValPx + 7.0)
getNewHDirectionAndDist LeftDir distValPx _ = if distValPx <= 0.0
                            then Tuple RightDir distValPx
                            else Tuple LeftDir (distValPx - 7.0)

getNewVDirectionAndDist :: VerticalDirection -> Number -> Int -> Tuple VerticalDirection Number
getNewVDirectionAndDist DownDir distValPx height = if distValPx >= (toNumber height) - 100.0
                            then Tuple UpDir distValPx
                            else Tuple DownDir (distValPx + 7.0)
getNewVDirectionAndDist UpDir distValPx _ = if distValPx <= 0.0
                            then Tuple DownDir distValPx
                            else Tuple UpDir (distValPx - 7.0)

moveBoxHorizontal :: HorizontalDirection -> Number -> Element.Element -> Effect Unit
moveBoxHorizontal RightDir distValPx el = do
                                w <- window
                                _ <- setStyleProp "left" distStr el
                                width <- innerWidth w
                                let tuple = getNewHDirectionAndDist RightDir distValPx width
                                let direction = fst tuple
                                    newDistVal = snd tuple
                                _ <- requestAnimationFrame (moveBoxHorizontal direction newDistVal el) w
                                pure unit
                                where distStr = (show distValPx) <> "px"

moveBoxHorizontal LeftDir distValPx el = do
                                w <- window
                                _ <- setStyleProp "left" distStr el
                                let tuple = getNewHDirectionAndDist LeftDir distValPx 0
                                let direction = fst tuple
                                    newDistVal = snd tuple
                                _ <- requestAnimationFrame (moveBoxHorizontal direction newDistVal el) w
                                pure unit
                                where distStr = (show distValPx) <> "px"

moveBoxVertical :: VerticalDirection -> Number -> Element.Element -> Effect Unit
moveBoxVertical DownDir distValPx el = do
                                w <- window
                                _ <- setStyleProp "top" distStr el
                                height <- innerHeight w
                                let tuple = getNewVDirectionAndDist DownDir distValPx height
                                let direction = fst tuple
                                    newDistVal = snd tuple
                                _ <- requestAnimationFrame (moveBoxVertical direction newDistVal el) w
                                pure unit
                                where distStr = (show distValPx) <> "px"
moveBoxVertical UpDir distValPx el = do
                                w <- window
                                _ <- setStyleProp "top" distStr el
                                height <- innerHeight w
                                let tuple = getNewVDirectionAndDist UpDir distValPx height
                                let direction = fst tuple
                                    newDistVal = snd tuple
                                _ <- requestAnimationFrame (moveBoxVertical direction newDistVal el) w
                                pure unit
                                where distStr = (show distValPx) <> "px"

execTimeout :: HorizontalDirection -> VerticalDirection -> Number -> Element.Element -> Effect Unit
execTimeout horizontalDir verticalDir distValPx el = do
                                        moveBoxHorizontal horizontalDir distValPx el
                                        moveBoxVertical   verticalDir distValPx el

main :: Effect Unit
main = do
  w <- window
  d <- document w
  mBody <- body d
  defaultElem <- (createElement "span" (toDocument d))
  let b = getBodyNodeFromMaybe d (Element.toNode (defaultElem)) mBody

  boxEl <- createBoxElement "the-box" $ toDocument d
  newBody <- appendChild (Element.toNode boxEl) b

  timeoutId <- requestAnimationFrame (execTimeout RightDir DownDir 10.0 boxEl) w
  _ <- setTimeout 1000 (cancelAnimationFrame timeoutId w)
  pure unit

