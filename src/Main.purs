module Main where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array ((!!), length)
import Effect (Effect)
import Effect.Random (randomInt)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)

import Web.DOM.Document (Document, createElement, toNonElementParentNode)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild, setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body, toDocument)
import Web.HTML.HTMLElement (HTMLElement, toNode)
import Web.HTML.Window (
    Window,
    RequestAnimationFrameId,
    document,
    cancelAnimationFrame,
    innerWidth,
    innerHeight,
    requestAnimationFrame,
    toEventTarget
)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key)

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

moveBox :: Direction -> RefPosition-> Element.Element -> String -> Ref RequestAnimationFrameId -> Effect Unit
moveBox dir positionRef{--(Tuple distValPxHor distValPxVert)--} el boxColor ref = do
                        w <- window
                        positionTuple <- read positionRef
                        let distValPxHor = fst positionTuple
                        let distValPxVert = snd positionTuple
                        let distValPx = if (dir == RightDir || dir == LeftDir)
                            then distValPxHor
                            else distValPxVert
                        let distStr = (show distValPx) <> "px"
                        let prop = (if (isHorizontal dir) then "left" else "top")
                        _ <- setStyleProp prop distStr el
                        width <- (\w' -> if dir == RightDir then w' else 0) <$> innerWidth w
                        height <- (\h -> if dir == DownDir then h else 0) <$> innerHeight w
                        tuple <- getNewDirectionAndDist dir distValPx (if isHorizontal dir then width else height) boxColor 
                        let direction = fst (fst tuple)
                            newDistVal = snd (fst tuple)
                            newColor = "#" <> (snd tuple)
                        let newPosition = if (dir == RightDir || dir == LeftDir)
                            then (Tuple newDistVal distValPxVert)
                            else (Tuple distValPxHor newDistVal)
                        success <- setStyleProp "background" newColor el -- change color

                        write newPosition positionRef
                        animationFrameId <- requestAnimationFrame (moveBox direction positionRef el newColor ref) w
                        write animationFrameId ref

type RefReqFrameId = Ref RequestAnimationFrameId
type RefPosition = Ref (Tuple Number Number)

execFrame :: Direction -> Direction -> RefPosition -> Element.Element ->
    (Tuple RefReqFrameId RefReqFrameId) -> Effect Unit
execFrame horizontalDir verticalDir distValPx el (Tuple refHor refVert) = do
                                        moveBox horizontalDir distValPx el defaultColor refHor
                                        moveBox verticalDir distValPx el defaultColor refVert


toggleBoxMove :: KeyboardEvent -> Ref Boolean -> (Tuple (Tuple RefReqFrameId RefReqFrameId) RefPosition) -> Effect Unit
toggleBoxMove keyEvent movingRef (Tuple (Tuple frameRefHor frameRefVert) positionRef) = case (key keyEvent) of
            "Enter" -> do
                isMoving <- read movingRef
                w <- window
                d <- map toDocument (document w)
                defaultElem <- (createElement "span" d)

                if isMoving == true
                    then read frameRefHor
                        >>= (flip cancelAnimationFrame) w
                        >>= pure (read frameRefVert) >>= ((flip cancelAnimationFrame) w)
                    else do
                        boxEl <- map (fromMaybe defaultElem) (getElementById "the-box" (toNonElementParentNode d))
                        
                        map (pure unit) (requestAnimationFrame (
                          execFrame RightDir DownDir positionRef boxEl
                              (Tuple frameRefHor frameRefVert)
                        ) w)

                write (not isMoving) movingRef
            _ -> pure unit


main :: Effect Unit
main = do
  w <- window
  d <- document w
  mBody <- body d
  defaultElem <- (createElement "span" (toDocument d))
  let b = getBodyNodeFromMaybe d (Element.toNode (defaultElem)) mBody

  boxEl <- createBoxElement "the-box" $ toDocument d
  newBody <- appendChild (Element.toNode boxEl) b

  -- Create frame that does nothing just to get default frame id
  defaultId <- (requestAnimationFrame (pure unit) w)
  frameRefHor <- new defaultId
  frameRefVert <- new defaultId
  positionRef <- new (Tuple 0.0 0.0)

  -- indicate if box is moving
  movingRef <- new true

  frameId <- requestAnimationFrame (
    execFrame RightDir DownDir positionRef boxEl
        (Tuple frameRefHor frameRefVert)
  ) w
  
  listener <- eventListener \e -> do
    case fromEvent e of
        Nothing -> pure unit
        Just keyEvent -> toggleBoxMove keyEvent movingRef (Tuple (Tuple frameRefHor frameRefVert) positionRef) 
  addEventListener (EventType "keydown") listener false (toEventTarget w)

  pure unit

