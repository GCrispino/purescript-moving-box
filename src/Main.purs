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

import Direction (Direction(..), isHorizontal, isVertical)

type Color = String

defaultColor = "ff4242" :: Color

colors = [
    defaultColor,
    "07f7af",
    "07c7f7",
    "fc982d",
    "2df5fc"
] :: Array Color

type State = { color :: Color
    , dirHor :: Direction
    , dirVert :: Direction
    , isMoving :: Boolean
    , position :: (Tuple Number Number)
    , rafIdHor :: RequestAnimationFrameId
    , rafIdVert :: RequestAnimationFrameId
}

getColor :: Effect Color 
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

getNewDirectionAndDist :: Direction -> Number -> Int -> Color -> Effect (Tuple (Tuple Direction Number) Color)
getNewDirectionAndDist dir distValPx widthOrHeight boxColor = case dir of 
                            RightDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then map (Tuple (Tuple LeftDir distValPx)) getColor
                                else pure $ Tuple (Tuple RightDir (distValPx + 9.0)) boxColor
                            LeftDir -> if distValPx <= 0.0
                                then map (Tuple (Tuple RightDir distValPx)) getColor
                                else pure $ Tuple (Tuple LeftDir (distValPx - 9.0)) boxColor
                            DownDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then map (Tuple (Tuple UpDir distValPx)) getColor
                                else pure $ Tuple (Tuple DownDir (distValPx + 9.0)) boxColor
                            UpDir -> if distValPx <= 0.0
                                then map (Tuple (Tuple DownDir distValPx)) getColor
                                else pure $ Tuple (Tuple UpDir (distValPx - 9.0)) boxColor

moveBox :: Direction -> Element.Element -> Ref State -> Effect Unit
moveBox dir el stateRef = do
                        w <- window
                        state <- read stateRef
                        let positionTuple = state.position
                            distValPxHor = fst positionTuple
                            distValPxVert = snd positionTuple
                            distValPx = if (dir == RightDir || dir == LeftDir)
                                then distValPxHor
                                else distValPxVert
                            distStr = (show distValPx) <> "px"
                            prop = (if (isHorizontal dir) then "left" else "top")
                        _ <- setStyleProp prop distStr el
                        width <- (\w' -> if dir == RightDir then w' else 0) <$> innerWidth w
                        height <- (\h -> if dir == DownDir then h else 0) <$> innerHeight w
                        tuple <- getNewDirectionAndDist dir distValPx (if isHorizontal dir then width else height) state.color
                        let direction = fst (fst tuple)
                            newDistVal = snd (fst tuple)
                            newColor = "#" <> (snd tuple)
                        let newPosition = if (dir == RightDir || dir == LeftDir)
                            then (Tuple newDistVal distValPxVert)
                            else (Tuple distValPxHor newDistVal)
                        success <- setStyleProp "background" newColor el -- change color

                        animationFrameId <- requestAnimationFrame (moveBox direction el stateRef) w

                        write { color: newColor
                            , dirHor: if (isHorizontal direction) then direction else state.dirHor
                            , dirVert: if (isVertical direction) then direction else state.dirVert
                            , isMoving: true
                            , position: newPosition
                            , rafIdHor: if (isHorizontal direction) then animationFrameId else state.rafIdHor
                            , rafIdVert: if (isVertical direction) then animationFrameId else state.rafIdVert
                        } stateRef

type RefReqFrameId = Ref RequestAnimationFrameId
type RefPosition = Ref (Tuple Number Number)

execFrame :: Direction -> Direction -> Element.Element -> Ref State -> Effect Unit
execFrame horizontalDir verticalDir el stateRef = do
                                        moveBox horizontalDir el stateRef
                                        moveBox verticalDir el stateRef


toggleBoxMove :: KeyboardEvent -> Ref State -> Effect Unit
toggleBoxMove keyEvent stateRef = case (key keyEvent) of
            "Enter" -> do
                state <- read stateRef
                w <- window
                d <- map toDocument (document w)
                defaultElem <- (createElement "span" d)

                if state.isMoving == true
                    then cancelAnimationFrame state.rafIdHor w
                        >>= pure (cancelAnimationFrame state.rafIdVert w)
                    else do
                        boxEl <- map (fromMaybe defaultElem) (getElementById "the-box" (toNonElementParentNode d))
                        
                        map (pure unit) (requestAnimationFrame (
                          execFrame state.dirHor state.dirVert boxEl stateRef
                        ) w)

                write { color: state.color
                    , dirHor: state.dirHor
                    , dirVert: state.dirVert
                    , isMoving: not state.isMoving                    
                    , position: state.position
                    , rafIdHor: state.rafIdHor
                    , rafIdVert: state.rafIdVert
                } stateRef
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

  stateRef <- new {
    color: defaultColor,
    dirHor: RightDir,
    dirVert: DownDir,
    isMoving: true,
    position: Tuple 0.0 0.0,
    rafIdHor: defaultId,
    rafIdVert: defaultId
  }

  frameId <- requestAnimationFrame (
    execFrame RightDir DownDir boxEl stateRef
  ) w
  
  listener <- eventListener \e -> do
    case fromEvent e of
        Nothing -> pure unit
        Just keyEvent -> toggleBoxMove keyEvent stateRef
  addEventListener (EventType "keydown") listener false (toEventTarget w)

  pure unit

