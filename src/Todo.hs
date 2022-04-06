{-# LANGUAGE OverloadedStrings #-}
module Todo where



-- imports ---------------------------------------------------------------------

-- internal
import qualified Types
import qualified UIHelp
import qualified Util

-- brick
import qualified Brick.Main    as BMain
import qualified Brick.Types   as BTypes
import qualified Brick.AttrMap as BAttr
import qualified Brick.Focus   as BFocus
import           Brick.Forms ((@@=))
import qualified Brick.Forms   as BForms

import           Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Core         as BWCore
import qualified Brick.Widgets.Center       as BWCentre
import qualified Brick.Widgets.Border       as BWBorder
import qualified Brick.Widgets.Border.Style as BWBStyle
import qualified Brick.Widgets.List         as BWList

import qualified Graphics.Vty.Attributes   as VtyAttr
import qualified Graphics.Vty.Input.Events as VtyEvents

-- microlens
import Lens.Micro
    ( (&) -- flipped $
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )
import qualified Lens.Micro    as Microlens
import qualified Lens.Micro.TH as MicrolensTH

-- sequence
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- misc
import qualified Data.Maybe as Maybe



--------------------------------------------------------------------------------

draw :: Types.AppState -> [BTypes.Widget Types.Name]
draw s = [UIHelp.screenBox s
    [BWList.renderList
        drawTodo
        (Types.getWidgetFocus s == Types.TodoList)
        (s ^. Types.todoState . Types.todoList)]]
  where
    drawTodo _ t 
        = case t ^. Types.todoPriority of
            Types.UrgentPriority -> 
                BWCore.withAttr Types.urgentPriorityAttr $ BWCore.str " * "
            Types.HighPriority -> 
                BWCore.withAttr Types.highPriorityAttr      $ BWCore.str " ! "
            Types.MediumPriority -> 
                BWCore.withAttr Types.mediumPriorityAttr    $ BWCore.str " : "
            Types.LowPriority -> 
                BWCore.withAttr Types.lowPriorityAttr       $ BWCore.str " . "
            Types.NoPriority -> BWCore.str " - "
        <+> BWCore.str (show t)

chooseCursor
    :: Types.AppState
    -> [BTypes.CursorLocation Types.Name]
    -> Maybe (BTypes.CursorLocation Types.Name)
chooseCursor _ _   = Nothing

todoListHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
todoListHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar ' ') [] -> BMain.continue
        $ s
        & Types.todoState . Types.todoList
        %~ BWList.listModify
        (Types.todoDone %~ not)

    VtyEvents.EvKey VtyEvents.KBS [] -> BMain.continue
        $ s
        & Types.todoState . Types.todoList
        %~ \l -> case BWList.listSelected l of
            Just i  -> BWList.listRemove i l
            Nothing -> l

    e -> do
        newTodoList <- BWList.handleListEventVi
            BWList.handleListEvent
            e
            (s ^. Types.todoState . Types.todoList)
        BMain.continue
            $ s
            & Types.todoState . Types.todoList
            .~ newTodoList

todoListHandleEvent s _ = BMain.continue s

handleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
handleEvent = todoListHandleEvent

handleCommand
    :: Types.AppState 
    -> Types.Command
    -> Types.AppState
handleCommand s (Types.NewTodoCommand n p)
    = s & Types.todoState . Types.todoList . BWList.listElementsL 
    %~ \l -> Seq.insertAt 
        (Maybe.fromMaybe 
            (Seq.length l) 
            (Seq.findIndexL (\t -> t ^. Types.todoPriority < p) l)) 
        (Types.Todo n False p) 
        l

handleCommand s (Types.MarkTodoCommand n d) = undefined
handleCommand s _ = s

focusRing :: BFocus.FocusRing Types.Name
focusRing = BFocus.focusRing [Types.TodoList]