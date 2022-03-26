module TodoList where

import Brick.Main (continue)
import Brick.Types 
    ( Widget
    , BrickEvent
    , EventM
    , Next
    , CursorLocation
    )
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (str)

import AppData (AppState, AppEvent, ResourceName)

--------------------------------------------------------------------------------

draw :: AppState -> [Widget ResourceName]
draw s = [center $ str "epic todo list"]

chooseCursor 
    :: AppState 
    -> [CursorLocation ResourceName] 
    -> Maybe (CursorLocation ResourceName)
chooseCursor _ _ = Nothing

handleEvent 
    :: AppState 
    -> BrickEvent ResourceName AppEvent 
    -> EventM ResourceName (Next AppState)
handleEvent s _ = continue s