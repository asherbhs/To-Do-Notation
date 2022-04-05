{-# LANGUAGE OverloadedStrings #-}

module UI where

-- imports ---------------------------------------------------------------------

-- internal --------------------------------------------------------------------
import qualified Types
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

import qualified Graphics.Vty.Attributes   as VtyAttr
import qualified Graphics.Vty.Input.Events as VtyEvents

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

-- stuff used  -----------------------------------------------------------------

screenMap :: Map Types.ScreenName Types.ScreenData
screenMap = Map.fromList
    [ ( Types.TodoScreen
      , Types.ScreenData
            { Types.draw         = Todo.draw
            , Types.chooseCursor = Todo.chooseCursor
            , Types.handleEvent  = Todo.handleEvent
            }
      )
    , ( Types.HabitScreen
      , Types.ScreenData
            { Types.draw         = Habit.draw
            , Types.chooseCursor = Habit.chooseCursor
            , Types.handleEvent  = Habit.handleEvent
            }
      )
    ]

defAttrMap :: Types.AppState -> BAttr.AttrMap
defAttrMap _ = BAttr.attrMap VtyAttr.defAttr
    [ ( BWList.listSelectedFocusedAttr
      , defaultStandout
      )
    ]
  where
    defaultStandout = VtyAttr.withStyle VtyAttr.defAttr VtyAttr.standout

defaultHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
defaultHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar 'c') [VtyEvents.MCtrl] -> BMain.halt s
    VtyEvents.EvKey VtyEvents.KBackTab [] -> BMain.continue
        $ s
        & Types.screenFocusRing
        %~ BFocus.focusNext
    e -> Types.handleEvent (screenMap ! Types.getScreen s) s (BTypes.VtyEvent e)

defaultHandleEvent s e = Types.handleEvent (screenMap ! Types.getScreen s) s e

debugHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
debugHandleEvent s e = defaultHandleEvent (s & Types.debug .~ show e) e

app :: BMain.App Types.AppState Types.AppEvent Types.Name
app = BMain.App
    { BMain.appDraw         = \s -> Types.draw         (getScreenData s) s
    , BMain.appChooseCursor = \s -> Types.chooseCursor (getScreenData s) s
    , BMain.appHandleEvent  = debugHandleEvent
    , BMain.appStartEvent   = return
    , BMain.appAttrMap      = defAttrMap
    }
  where
    getScreenData s = screenMap ! Types.getScreen s

ui :: IO ()
ui = void $ do
    json <- ByteString.readFile "todos"

    finalState <- BMain.defaultMain app Types.AppState
        { Types._debug = "[DEBUG]"
        , Types._screenFocusRing = BFocus.focusRing
            [ Types.TodoScreen
            , Types.HabitScreen
            ]
        , Types._todoState = Types.TodoState
            { Types._todoList = BWList.list
                Types.TodoList
                (
                Maybe.fromMaybe
                    Seq.empty
                    $ Aeson.decode json
                )
                1
            , Types._todoFocusRing = BFocus.focusRing [Types.TodoList]
            }
        }

    ByteString.writeFile "todos"
        $ Aeson.encode
        $ BWList.listElements
        $ finalState ^. Types.todoState . Types.todoList
