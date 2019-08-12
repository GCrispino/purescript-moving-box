module Main where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Array ((!!), filter, length)
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

import Direction (Direction(..), ifIsHorizontal, ifIsVertical)

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
    , rafId :: RequestAnimationFrameId
}

getColor :: Color -> Effect Color 
getColor curColor = map 
            ((fromMaybe defaultColor) <<< ((!!) colors'))
            (randomInt 0 $ (length colors') - 1)
            where colors' = filter (\c -> c /= curColor) colors

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
    _ <- setStyleProp "position" "relative" boxEl
    _ <- setStyleProp "width" "5em" boxEl
    _ <- setStyleProp "height" "5em" boxEl
    _ <- setStyleProp "background" "#ff4242" boxEl
    pure boxEl

getBodyNodeFromMaybe :: HTMLDocument -> Node -> Maybe HTMLElement -> Node
getBodyNodeFromMaybe d defaultNode mB = case mB of 
    Nothing -> defaultNode
    Just b -> toNode b

getNewDirectionAndDist :: Direction -> Number -> Int -> Effect (Tuple Direction Number)
getNewDirectionAndDist dir distValPx widthOrHeight = case dir of 
                            RightDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then pure (Tuple LeftDir distValPx)
                                else pure $ Tuple RightDir (distValPx + 9.0)
                            LeftDir -> if distValPx <= 0.0
                                then pure (Tuple RightDir distValPx)
                                else pure $ Tuple LeftDir (distValPx - 9.0)
                            DownDir -> if distValPx >= (toNumber widthOrHeight) - 100.0
                                then pure (Tuple UpDir distValPx)
                                else pure $ Tuple DownDir (distValPx + 9.0)
                            UpDir -> if distValPx <= 0.0
                                then pure (Tuple DownDir distValPx)
                                else pure $ Tuple UpDir (distValPx - 9.0)

moveBox :: Direction -> Direction -> Element.Element -> Ref State -> Effect Unit
moveBox hDir vDir el stateRef = do
                        -- Read state
                        state <- read stateRef

                        -- Move box
                        let (Tuple distValPxHor distValPxVert) = state.position
                            distStrHor = (show distValPxHor) <> "px"
                            distStrVert = (show distValPxVert) <> "px"
                        _ <- setStyleProp "transform" ( "translate(" <> distStrHor <> ", " <> distStrVert <> ")" ) el

                        -- Determine new direction
                        w <- window
                        width <- innerWidth w
                        height <- innerHeight w
                        Tuple hDirection newHDist <- getNewDirectionAndDist hDir distValPxHor width
                        Tuple vDirection newVDist <- getNewDirectionAndDist vDir distValPxVert height
                        let newPosition = Tuple newHDist newVDist

                        -- change color
                        newColor <- if changedDir (hDirection /= hDir) || (vDirection /= vDir)
                            then getColor state.color
                            else pure state.color

                        success <- setStyleProp "background" ("#" <> newColor) el

                        -- Call next frame
                        animationFrameId <- requestAnimationFrame (moveBox hDirection vDirection el stateRef) w

                        -- Update state
                        write { color: newColor
                            , dirHor: hDirection
                            , dirVert: vDirection
                            , isMoving: true
                            , position: newPosition
                            , rafId: animationFrameId
                        } stateRef

type RefReqFrameId = Ref RequestAnimationFrameId
type RefPosition = Ref (Tuple Number Number)

execFrame :: Direction -> Direction -> Element.Element -> Ref State -> Effect Unit
execFrame hDir vDir el stateRef = do
                                        moveBox hDir vDir el stateRef


toggleBoxMove :: KeyboardEvent -> Ref State -> Effect Unit
toggleBoxMove keyEvent stateRef = case (key keyEvent) of
            "Enter" -> do
                state <- read stateRef
                w <- window
                d <- map toDocument (document w)
                defaultElem <- (createElement "span" d)

                if state.isMoving == true
                    then cancelAnimationFrame state.rafId w
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
                    , rafId: state.rafId
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
    rafId: defaultId
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

