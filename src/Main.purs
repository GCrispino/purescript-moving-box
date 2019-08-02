module Main where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array ((!!), length)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Effect.Random (randomInt)
import Effect.Console (log)

import Web.DOM.Document (Document, createElement)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild, setTextContent)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body, toDocument)
import Web.HTML.HTMLElement (HTMLElement, toNode)
import Web.HTML.Window (
    document,
    cancelAnimationFrame,
    innerWidth,
    innerHeight,
    requestAnimationFrame,
    toEventTarget
)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

import Direction (Direction(..), isHorizontal)

defaultColor = "ff4242" :: String

colors = [
    defaultColor,
    "07f7af",
    "07c7f7",
    "fc982d",
    "2df5fc"
] :: Array String

getColor :: Effect String 
getColor = map 
            ((fromMaybe defaultColor) <<< ((!!) colors)) 
            (randomInt 0 (length colors))

createElementWithContent :: String -> String -> HTMLDocument -> Effect Element.Element 
createElementWithContent tag content d = do 
      el <- createElement tag (toDocument d)
      setTextContent content (Element.toNode el)
      pure el

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

getNewDirectionAndDist :: Direction -> Number -> Int -> String -> Effect (Tuple (Tuple Direction Number) String)
getNewDirectionAndDist dir distValPx widthOrHeight boxColor = case dir of 
                            RightDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then map (Tuple (Tuple LeftDir distValPx)) getColor
                                else pure $ Tuple (Tuple RightDir (distValPx + 8.0)) boxColor
                            LeftDir -> if distValPx <= 0.0
                                then map (Tuple (Tuple RightDir distValPx)) getColor
                                else pure $ Tuple (Tuple LeftDir (distValPx - 8.0)) boxColor
                            DownDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then map (Tuple (Tuple UpDir distValPx)) getColor
                                else pure $ Tuple (Tuple DownDir (distValPx + 8.0)) boxColor
                            UpDir -> if distValPx <= 0.0
                                then map (Tuple (Tuple DownDir distValPx)) getColor
                                else pure $ Tuple (Tuple UpDir (distValPx - 8.0)) boxColor

moveBox :: Direction -> Number -> Element.Element -> String -> Effect Unit
moveBox dir distValPx el boxColor = do
                        w <- window
                        let prop = (if (isHorizontal dir) then "left" else "top")
                        _ <- setStyleProp prop distStr el
                        width <- (\w' -> if dir == RightDir then w' else 0) <$> innerWidth w
                        height <- (\h -> if dir == DownDir then h else 0) <$> innerHeight w
                        tuple <- getNewDirectionAndDist dir distValPx (if isHorizontal dir then width else height) boxColor 
                        let direction = fst (fst tuple)
                            newDistVal = snd (fst tuple)
                            newColor = "#" <> (snd tuple)
                        success <- setStyleProp "background" newColor el -- change color

                        _ <- requestAnimationFrame (moveBox direction newDistVal el newColor) w
                        pure unit
                        where distStr = (show distValPx) <> "px"

execFrame :: Direction -> Direction -> Number -> Element.Element -> Effect Unit
execFrame horizontalDir verticalDir distValPx el = do
                                        moveBox horizontalDir distValPx el defaultColor 
                                        moveBox verticalDir distValPx el defaultColor 

main :: Effect Unit
main = do
  w <- window
  d <- document w
  mBody <- body d
  defaultElem <- (createElement "span" (toDocument d))
  let b = getBodyNodeFromMaybe d (Element.toNode (defaultElem)) mBody

  boxEl <- createBoxElement "the-box" $ toDocument d
  newBody <- appendChild (Element.toNode boxEl) b

  timeoutId <- requestAnimationFrame (execFrame RightDir DownDir 10.0 boxEl) w
  _ <- setTimeout 1000 (cancelAnimationFrame timeoutId w)
  
  listener <- eventListener \e -> do
    case fromEvent e of
        Nothing -> pure unit
        Just keyEvent -> if (key keyEvent) == "Enter"
            then (log "cancelAnimationFrame!!!") 
            else pure unit
            
  addEventListener (EventType "keydown") listener false (toEventTarget w)

  pure unit

