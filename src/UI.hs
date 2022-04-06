{-# LANGUAGE OverloadedStrings #-}

module UI where

-- imports ---------------------------------------------------------------------

-- internal --------------------------------------------------------------------
import qualified Types
import qualified Parser
import qualified Todo
import qualified Habit

-- language features
import GHC.Generics

-- data types
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- ui
import qualified Brick.Main    as BMain
import qualified Brick.Types   as BTypes
import qualified Brick.AttrMap as BAttr
import qualified Brick.Focus   as BFocus

import           Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Core         as BWCore
import qualified Brick.Widgets.Center       as BWCentre
import qualified Brick.Widgets.Border       as BWBorder
import qualified Brick.Widgets.Border.Style as BWBStyle
import qualified Brick.Widgets.List         as BWList
import qualified Brick.Widgets.Edit         as BWEdit

import qualified Graphics.Vty.Attributes       as VtyAttr
import qualified Graphics.Vty.Attributes.Color as VtyColour
import qualified Graphics.Vty.Input.Events     as VtyEvents

-- JSON
import qualified Data.Aeson as Aeson

-- microlens
import Lens.Micro
    ( (&) -- flipped $
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )
import qualified Lens.Micro    as Microlens
import qualified Lens.Micro.TH as MicrolensTH

-- odds and ends
import Control.Monad (void)
import qualified Data.Maybe as Maybe
import qualified Data.Bool  as Bool
import Data.CircularList (insertR)

-- stuff used  -----------------------------------------------------------------

screenMap :: Map Types.ScreenName Types.ScreenData
screenMap = Map.fromList
    [ ( Types.TodoScreen
      , Types.ScreenData
            { Types.draw          = Todo.draw
            , Types.chooseCursor  = Todo.chooseCursor
            , Types.handleEvent   = Todo.handleEvent
            , Types.handleCommand = Todo.handleCommand
            , Types.focusRing     = Todo.focusRing
            }
      )
    , ( Types.HabitScreen
      , Types.ScreenData
            { Types.draw          = Habit.draw
            , Types.chooseCursor  = Habit.chooseCursor
            , Types.handleEvent   = Habit.handleEvent
            , Types.handleCommand = Habit.handleCommand
            , Types.focusRing     = Habit.focusRing
            }
      )
    ]

getScreenData :: Types.AppState -> Types.ScreenData
getScreenData = (!) screenMap . Types.getScreen
    --Maybe.fromJust $ Map.lookup (Types.getScreen s) screenMap
    --screenMap ! Types.getScreen s

defAttrMap :: Types.AppState -> BAttr.AttrMap
defAttrMap _ = BAttr.attrMap VtyAttr.defAttr
    [ ( BWList.listSelectedFocusedAttr
      , defaultStandout
      )
    , ( Types.highPriorityAttr
      , VtyAttr.defAttr `VtyAttr.withForeColor` VtyColour.red
      )
    , ( Types.mediumPriorityAttr
      , VtyAttr.defAttr `VtyAttr.withForeColor` VtyColour.yellow
      )
    , ( Types.lowPriorityAttr
      , VtyAttr.defAttr `VtyAttr.withForeColor` VtyColour.green
      )
    ]
  where
    defaultStandout = VtyAttr.withStyle VtyAttr.defAttr VtyAttr.standout

defaultChooseCursor
    :: Types.AppState
    -> [BTypes.CursorLocation Types.Name]
    -> Maybe (BTypes.CursorLocation Types.Name)
defaultChooseCursor s [l] = Just l
defaultChooseCursor s ls  = Types.chooseCursor (getScreenData s) s ls

screenHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
screenHandleEvent s = Types.handleEvent (getScreenData s) s

commandPromptHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
commandPromptHandleEvent s (BTypes.VtyEvent (VtyEvents.EvKey VtyEvents.KEnter []))
    = BMain.continue
    $ case cmd of
        Left e  -> s & Types.errorMessage .~ e
        Right c -> 
            Types.handleCommand (getScreenData s) s c
            & Types.errorMessage .~ ""

    & Types.commandPrompt
    .~ emptyCommandPrompt
  where 
    cmd = Parser.parseCommand
        $ head 
        $ BWEdit.getEditContents 
        $ s ^. Types.commandPrompt

commandPromptHandleEvent s (BTypes.VtyEvent e) = do
    newEditor <- BWEdit.handleEditorEvent e $ s ^. Types.commandPrompt
    BMain.continue 
        $ s
        & Types.commandPrompt
        .~ newEditor
commandPromptHandleEvent s e = screenHandleEvent s e

defaultHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
defaultHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar 'c') [VtyEvents.MCtrl] -> BMain.halt s
    VtyEvents.EvKey (VtyEvents.KChar '\t') [] -> BMain.continue
        $ s
        & Types.widgetFocusRing
        %~ BFocus.focusNext
    VtyEvents.EvKey VtyEvents.KBackTab [] -> BMain.continue
        $ s
        & Types.screenFocusRing
        %~ BFocus.focusNext
    _ -> if Types.getWidgetFocus s == Types.CommandPrompt
         then commandPromptHandleEvent s (BTypes.VtyEvent e)
         else screenHandleEvent        s (BTypes.VtyEvent e)

defaultHandleEvent s e = screenHandleEvent s e

debugHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
debugHandleEvent s e = defaultHandleEvent (s & Types.debug .~ show e) e

app :: BMain.App Types.AppState Types.AppEvent Types.Name
app = BMain.App
    { BMain.appDraw         = \s -> Types.draw         (getScreenData s) s
    , BMain.appChooseCursor = defaultChooseCursor
    , BMain.appHandleEvent  = debugHandleEvent
    , BMain.appStartEvent   = return
    , BMain.appAttrMap      = defAttrMap
    }

emptyCommandPrompt :: BWEdit.Editor Text Types.Name
emptyCommandPrompt = BWEdit.editorText 
    Types.CommandPrompt
    (Just 1)
    ""

ui :: IO ()
ui = void $ do
    json <- ByteString.readFile "todos"

    finalState <- BMain.defaultMain app Types.AppState
        { Types._debug = "[DEBUG]"
        , Types._widgetFocusRing 
            = BFocus.focusRingModify (insertR Types.CommandPrompt)
            $ Types.focusRing 
            $ screenMap ! Types.TodoScreen
        , Types._screenFocusRing = BFocus.focusRing
            [ Types.TodoScreen
            , Types.HabitScreen
            ]
        , Types._commandPrompt = emptyCommandPrompt
        , Types._errorMessage = ""
        , Types._todoState = Types.TodoState
            { Types._todoList = BWList.list
                Types.TodoList
                (
                Maybe.fromMaybe
                    Seq.empty
                    $ Aeson.decode json
                -- Seq.fromList
                --     [ Types.Todo "rip out todo form" True 0
                --     , Types.Todo "put a command prompt in the wrapper box" False 0
                --     , Types.Todo "bind commands to functionality" False 0
                --     , Types.Todo "show and order by priority" False 0
                --     ]
                )
                1
            , Types._todoFocusRing = BFocus.focusRing [Types.TodoList]
            }
        }

    ByteString.writeFile "todos"
        $ Aeson.encode
        $ BWList.listElements
        $ finalState ^. Types.todoState . Types.todoList
