module Habit where

import qualified Types
import qualified UIHelp

import qualified Brick.Types        as BTypes
import qualified Brick.Main         as BMain
import qualified Brick.Focus        as BFocus
import qualified Brick.Widgets.Core as BWCore

draw :: Types.AppState -> [BTypes.Widget Types.Name]
draw s =
    [ UIHelp.screenBox s
        [ BWCore.str "habitss"
        ]
    ]

chooseCursor
     :: Types.AppState
    -> [BTypes.CursorLocation Types.Name]
    -> Maybe (BTypes.CursorLocation Types.Name)
chooseCursor _ _ = Nothing

handleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
handleEvent s _ = BMain.continue s

handleCommand
    :: Types.AppState 
    -> Types.Command
    -> Types.AppState
handleCommand = const

focusRing :: BFocus.FocusRing Types.Name
focusRing = BFocus.focusRing [Types.HabitList]
