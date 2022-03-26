module AppData where

import Brick.Types 
    ( Widget
    , CursorLocation
    , BrickEvent
    , EventM
    , Next
    )

import Brick.AttrMap
    ( AttrMap
    , attrMap
    )

import Graphics.Vty.Attributes (defAttr)

--------------------------------------------------------------------------------

data AppState = AppState 
    { screen :: ScreenName }

type AppEvent = ()

data ScreenName 
    = TodoListScreen
    | EventScreen
    | HabitScreen
    | TimelineScreen
    deriving (Eq, Ord, Show)

data ScreenData = ScreenData
    { draw
        :: AppState
        -> [Widget ResourceName]
    , chooseCursor
        :: AppState
        -> [CursorLocation ResourceName]
        -> Maybe (CursorLocation ResourceName)
    , handleEvent
        :: AppState
        -> BrickEvent ResourceName AppEvent
        -> EventM ResourceName (Next AppState)
    }

defAttrMap :: AppState -> AttrMap
defAttrMap _ = attrMap defAttr []

data ResourceName
    = None
    deriving (Eq, Ord, Show)