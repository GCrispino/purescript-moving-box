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

import Web.DOM.Document (Document, createElement, toNonElementParentNode) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (Node, appendChild, setTextContent) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.Event.Event (EventType(..)) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (HTMLDocument, body, toDocument) as HTML
import Web.HTML.HTMLElement as HTML.HTMLElement
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key) as Event

import Direction (Direction(..))

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
    , rafId :: HTML.Window.RequestAnimationFrameId
}

getColor :: Color -> Effect Color 
getColor curColor = map 
            ((fromMaybe defaultColor) <<< ((!!) colors'))
            (randomInt 0 $ (length colors') - 1)
            where colors' = filter (\c -> c /= curColor) colors

createElementWithContent :: String -> String -> HTML.HTMLDocument -> Effect DOM.Element.Element 
createElementWithContent tag content d = do 
      el <- DOM.createElement tag (HTML.toDocument d)
      DOM.setTextContent content (DOM.Element.toNode el)
      pure el

foreign import setStyleProp :: String -> String -> DOM.Element.Element -> Effect Boolean

createBoxElement :: String -> DOM.Document -> Effect DOM.Element.Element
createBoxElement id document = do
    boxEl <- DOM.createElement "div" document
    DOM.Element.setId id boxEl
    DOM.Element.setClassName "box" boxEl
    _ <- setStyleProp "position" "relative" boxEl
    _ <- setStyleProp "width" "5em" boxEl
    _ <- setStyleProp "height" "5em" boxEl
    _ <- setStyleProp "background" "#ff4242" boxEl
    pure boxEl

getBodyNodeFromMaybe :: HTML.HTMLDocument -> DOM.Node -> Maybe HTML.HTMLElement.HTMLElement -> DOM.Node
getBodyNodeFromMaybe d defaultNode mB = case mB of 
    Nothing -> defaultNode
    Just b -> HTML.HTMLElement.toNode b

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

moveBox :: Direction -> Direction -> DOM.Element.Element -> Ref State -> Effect Unit
moveBox hDir vDir el stateRef = do
                        -- Read state
                        state <- read stateRef

                        -- Move box
                        let (Tuple distValPxHor distValPxVert) = state.position
                            distStrHor = (show distValPxHor) <> "px"
                            distStrVert = (show distValPxVert) <> "px"
                        _ <- setStyleProp "transform" ( "translate(" <> distStrHor <> ", " <> distStrVert <> ")" ) el

                        -- Determine new direction
                        w <- HTML.window
                        width <- HTML.Window.innerWidth w
                        height <- HTML.Window.innerHeight w
                        Tuple hDirection newHDist <- getNewDirectionAndDist hDir distValPxHor width
                        Tuple vDirection newVDist <- getNewDirectionAndDist vDir distValPxVert height
                        let newPosition = Tuple newHDist newVDist

                        -- change color
                        newColor <- if (hDirection /= hDir) || (vDirection /= vDir)
                            then getColor state.color
                            else pure state.color

                        success <- setStyleProp "background" ("#" <> newColor) el

                        -- Call next frame
                        animationFrameId <- HTML.Window.requestAnimationFrame (moveBox hDirection vDirection el stateRef) w

                        -- Update state
                        write { color: newColor
                            , dirHor: hDirection
                            , dirVert: vDirection
                            , isMoving: true
                            , position: newPosition
                            , rafId: animationFrameId
                        } stateRef

type RefReqFrameId = Ref HTML.Window.RequestAnimationFrameId
type RefPosition = Ref (Tuple Number Number)

execFrame :: Direction -> Direction -> DOM.Element.Element -> Ref State -> Effect Unit
execFrame hDir vDir el stateRef = moveBox hDir vDir el stateRef

toggleBoxMove :: Event.KeyboardEvent -> Ref State -> Effect Unit
toggleBoxMove keyEvent stateRef = case (Event.key keyEvent) of
            "Enter" -> do
                state <- read stateRef
                w <- HTML.window
                d <- map HTML.toDocument (HTML.Window.document w)
                defaultElem <- (DOM.createElement "span" d)

                if state.isMoving == true
                    then HTML.Window.cancelAnimationFrame state.rafId w
                    else do
                        boxEl <- map (fromMaybe defaultElem) (DOM.getElementById "the-box" (DOM.toNonElementParentNode d))
                        
                        map (pure unit) (HTML.Window.requestAnimationFrame (
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
  w <- HTML.window
  d <- HTML.Window.document w
  mBody <- HTML.body d
  defaultElem <- (DOM.createElement "span" (HTML.toDocument d))
  let b = getBodyNodeFromMaybe d (DOM.Element.toNode (defaultElem)) mBody

  boxEl <- createBoxElement "the-box" $ HTML.toDocument d
  newBody <- DOM.appendChild (DOM.Element.toNode boxEl) b

  -- Create frame that does nothing just to get default frame id
  defaultId <- (HTML.Window.requestAnimationFrame (pure unit) w)

  stateRef <- new {
    color: defaultColor,
    dirHor: RightDir,
    dirVert: DownDir,
    isMoving: true,
    position: Tuple 0.0 0.0,
    rafId: defaultId
  }

  frameId <- HTML.Window.requestAnimationFrame (
    execFrame RightDir DownDir boxEl stateRef
  ) w
  
  listener <- Event.eventListener \e -> do
    case Event.fromEvent e of
        Nothing -> pure unit
        Just keyEvent -> toggleBoxMove keyEvent stateRef
  Event.addEventListener (Event.EventType "keydown") listener false (HTML.Window.toEventTarget w)

