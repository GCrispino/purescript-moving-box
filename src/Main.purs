module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple
import Effect (Effect)
import Effect.Console (log)
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

getNewDirectionAndDist :: Direction -> Number -> Int -> Tuple Direction Number
getNewDirectionAndDist dir distValPx widthOrHeight = case dir of
                            RightDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then Tuple LeftDir distValPx
                                else Tuple RightDir (distValPx + 7.0)
                            LeftDir -> if distValPx <= 0.0
                                then Tuple RightDir distValPx
                                else Tuple LeftDir (distValPx - 7.0)
                            DownDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then Tuple UpDir distValPx
                                else Tuple DownDir (distValPx + 7.0)
                            UpDir -> if distValPx <= 0.0
                                then Tuple DownDir distValPx
                                else Tuple UpDir (distValPx - 7.0)

moveBox :: Direction -> Number -> Element.Element -> Effect Unit
moveBox dir distValPx el = do
                            w <- window
                            let prop = (if (isHorizontal dir) then "left" else "top")
                            _ <- setStyleProp prop distStr el
                            width <- (\w -> if dir == RightDir then w else 0) <$> innerWidth w
                            height <- (\h -> if dir == DownDir then h else 0) <$> innerHeight w
                            let tuple = getNewDirectionAndDist dir distValPx (if isHorizontal dir then width else height)
                                direction = fst tuple
                                newDistVal = snd tuple

                            _ <- requestAnimationFrame (moveBox direction newDistVal el) w
                            pure unit
                            where distStr = (show distValPx) <> "px"

execTimeout :: Direction -> Direction -> Number -> Element.Element -> Effect Unit
execTimeout horizontalDir verticalDir distValPx el = do
                                        moveBox horizontalDir distValPx el
                                        moveBox verticalDir distValPx el

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

