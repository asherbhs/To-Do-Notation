{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module UI where



-- imports ---------------------------------------------------------------------

-- internal
import qualified Types
import qualified Parser
import qualified Todo
import qualified Habit

-- language features
import GHC.Generics ()

-- data types
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.ByteString.Lazy as ByteString

import Data.Sequence ((<|))
import qualified Data.Sequence as Seq

-- ui
import qualified Brick.Main    as BMain
import qualified Brick.Types   as BTypes
import qualified Brick.AttrMap as BAttr
import qualified Brick.Focus   as BFocus

import qualified Brick.Widgets.List         as BWList
import qualified Brick.Widgets.Edit         as BWEdit

import qualified Graphics.Vty.Attributes       as VtyAttr
import qualified Graphics.Vty.Attributes.Color as VtyColour
import qualified Graphics.Vty.Input.Events     as VtyEvents

-- JSON
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty

-- microlens
import Lens.Micro
    ( (&)  -- flipped $ for piping
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )

-- time
import Data.Time as Time

-- odds and ends
import Control.Monad (void)
import qualified Data.Maybe as Maybe
import Data.CircularList (insertR)
import Data.Text.Zipper (gotoEOL)
import Data.List.Index (imap)



--------------------------------------------------------------------------------

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
getScreenData s = screenMap ! Types.getScreen s

defAttrMap :: Types.AppState -> BAttr.AttrMap
defAttrMap _ = BAttr.attrMap VtyAttr.defAttr
    [ ( BWList.listSelectedFocusedAttr
      , defaultStandout
      )
    , ( Types.extraPriorityAttr
      , withTextColour VtyColour.brightBlue
      )
    , ( Types.urgentPriorityAttr
      , withTextColour VtyColour.brightMagenta
      )
    , ( Types.highPriorityAttr
      , withTextColour VtyColour.brightRed
      )
    , ( Types.mediumPriorityAttr
      , withTextColour VtyColour.brightYellow
      )
    , ( Types.lowPriorityAttr
      , withTextColour VtyColour.brightGreen
      )
    ]
  where
    defaultStandout = VtyAttr.withStyle VtyAttr.defAttr VtyAttr.standout
    withTextColour = VtyAttr.withForeColor VtyAttr.defAttr

defaultChooseCursor
    :: Types.AppState
    -> [BTypes.CursorLocation Types.Name]
    -> Maybe (BTypes.CursorLocation Types.Name)
defaultChooseCursor _ [] = Nothing
defaultChooseCursor _ [l] = Just l
defaultChooseCursor s ls  = Types.chooseCursor (getScreenData s) s ls

screenHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
screenHandleEvent s = Types.handleEvent (getScreenData s) s

defaultCommandPrompt :: Text -> BWEdit.Editor Text Types.Name
defaultCommandPrompt = BWEdit.editorText Types.CommandPrompt (Just 1)

emptyCommandPrompt :: BWEdit.Editor Text Types.Name
emptyCommandPrompt = defaultCommandPrompt ""

commandPromptHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
commandPromptHandleEvent s (BTypes.VtyEvent e) =
    case e of
        VtyEvents.EvKey VtyEvents.KEnter [] -> if
            | Text.null cmdText -> BMain.continue $ s & Types.errorMessage .~ ""
            | cmd == Right Types.QuitCommand -> BMain.halt s
            | otherwise -> BMain.continue
                $ case cmd of
                    Left err -> s & Types.errorMessage .~ err
                    Right c -> defaultHandleCommand s c
                & Types.previousCommands %~ (cmdText <|)
                & Types.commandPrompt .~ emptyCommandPrompt

        VtyEvents.EvKey VtyEvents.KUp [] -> BMain.continue $ s
            & Types.previousCommandIndex %~ (\i ->
                min (i + 1) (Seq.length (s ^. Types.previousCommands) - 1))
            & loadOldCommand

        VtyEvents.EvKey VtyEvents.KDown [] -> BMain.continue $ s
            & Types.previousCommandIndex %~ (\i -> max (i - 1) (-1))
            & loadOldCommand

        _ -> do
            newEditor <- BWEdit.handleEditorEvent e $ s ^. Types.commandPrompt
            BMain.continue $ s & Types.commandPrompt .~ newEditor
  where
    loadOldCommand s' = s'
        & Types.commandPrompt .~
            if s' ^. Types.previousCommandIndex == -1
            then emptyCommandPrompt
            else defaultCommandPrompt (Seq.index
                    (s' ^. Types.previousCommands)
                    (s' ^. Types.previousCommandIndex))
                & BWEdit.editContentsL %~ gotoEOL

    cmd = Parser.parseCommand cmdText
    cmdText = head $ BWEdit.getEditContents $ s ^. Types.commandPrompt
commandPromptHandleEvent s e = screenHandleEvent s e

defaultHandleCommand
    :: Types.AppState
    -> Types.Command
    -> Types.AppState
defaultHandleCommand s Types.HelpCommand = s & Types.errorMessage .~
    "HELP:\n\
    \\n\
    \Press TAB to shift focus\n\
    \Press SHIFT + TAB to shift screen\n"
  -- 0---------1---------2---------3---------4---------5---------6---------7---------
    `Text.append` case Types.getScreen s of
        Types.TodoScreen ->
            "\n\
            \In the to-do list:\n\
            \    Press BACKSPACE to delete a to-do\n\
            \    Press SPACE to mark (or unmark) a to-do as done\n\
            \\n\
            \Commands:\n\
            \    new todo [NAME] [PRIORITY]\n\
            \        [NAME]     - the name of the new to-do\n\
            \        [PRIORITY] - optional, the to-do's urgency\n\
            \                     \"low\", \"medium\", \"high\", or \"urgent\"\n"
          -- 0---------1---------2---------3---------4---------5---------6---------7---------
        _ -> "\n"
    `Text.append`
        "    quit\n\
        \\n\
        \To include whitespace in a string argument, enclose it in \"double quotes\""
      -- 0---------1---------2---------3---------4---------5---------6---------7---------
defaultHandleCommand s c = Types.handleCommand
    (getScreenData s)
    (s & Types.errorMessage .~ "")
    c

defaultHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
defaultHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar 'c') [VtyEvents.MCtrl] -> BMain.halt s

    VtyEvents.EvKey (VtyEvents.KChar '\t') [] -> BMain.continue $ s
        & Types.widgetFocusRing %~ BFocus.focusNext

    VtyEvents.EvKey VtyEvents.KBackTab [] -> BMain.continue $ s
        & Types.screenFocusRing %~ BFocus.focusNext

        & (\s' -> s' & Types.widgetFocusRing
            .~  BFocus.focusRingModify
                (insertR Types.CommandPrompt)
                (Types.focusRing $ screenMap ! Types.getScreen s')
        )

    _ -> if Types.getWidgetFocus s == Types.CommandPrompt
        then commandPromptHandleEvent s (BTypes.VtyEvent e)
        else screenHandleEvent        s (BTypes.VtyEvent e)

defaultHandleEvent s e = screenHandleEvent s e

debugHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
debugHandleEvent s e = defaultHandleEvent (s & Types.debug .~ show (Types.getWidgetFocus s)) e

app :: BMain.App Types.AppState Types.AppEvent Types.Name
app = BMain.App
    { BMain.appDraw         = \s -> Types.draw (getScreenData s) s
    , BMain.appChooseCursor = defaultChooseCursor
    , BMain.appHandleEvent  = debugHandleEvent
    , BMain.appStartEvent   = return
    , BMain.appAttrMap      = defAttrMap
    }

ui :: IO ()
ui = void $ do
    currentTime <- Time.getCurrentTime
    jsonTodos <- ByteString.readFile "todos"
    jsonHabits <- ByteString.readFile "habits"

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
        , Types._commandPrompt        = emptyCommandPrompt
        , Types._previousCommands     = Seq.empty
        , Types._previousCommandIndex = -1
        , Types._errorMessage         = ""
        , Types._today                = Time.utctDay currentTime
        , Types._todoState = Types.TodoState
            { Types._todoList = BWList.list
                Types.TodoList
                (Maybe.fromMaybe
                    Seq.empty
                    (Aeson.decode jsonTodos))
                1
            , Types._todoFocusRing = BFocus.focusRing [Types.TodoList]
            }
        , Types._habitState = Types.HabitState
            { Types._habitList = imap
                (\i h -> (h, Habit.habitDaysList h (Time.utctDay currentTime) i))
                (Maybe.fromMaybe
                    []
                    (Aeson.decode jsonHabits))
            , Types._habitFocus = 0
            }
        }

    ByteString.writeFile "todos"
        $ AesonPretty.encodePretty
        $ BWList.listElements
        $ finalState ^. Types.todoState . Types.todoList

    ByteString.writeFile "habits"
        $ AesonPretty.encodePretty
        $ map fst (finalState ^. Types.habitState . Types.habitList)
