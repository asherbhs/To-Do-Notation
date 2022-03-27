{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module UI where

-- imports ---------------------------------------------------------------------

-- language features
import GHC.Generics

-- data types
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- ui
import qualified Brick.Main    as BMain
import qualified Brick.Types   as BTypes
import qualified Brick.AttrMap as BAttr
import qualified Brick.Focus   as BFocus

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

-- stuff used  -----------------------------------------------------------------

type AppEvent = ()

data Name
    = None
    | TodoList
    deriving (Eq, Ord, Show)

data ScreenName 
    = TodoScreen
    | EventScreen
    | HabitScreen
    | TimelineScreen
    deriving (Eq, Ord, Show)

data Todo = Todo
    { _name :: Text
    , _done :: Bool
    } deriving (Eq, Generic)

MicrolensTH.makeLenses '' Todo

instance Aeson.ToJSON Todo where
instance Aeson.FromJSON Todo where

instance Show Todo where
    show t 
        = '[' : (if t ^. done then '/' else ' ') : ']' : ' ' 
        : (Text.unpack $ t ^. name)

instance BWList.Splittable [] where
    splitAt n xs = (take n xs, drop n xs)

data TodoState = TodoState
    { _todoList          :: BWList.GenericList Name [] Todo
    , _todoListFocusRing :: BFocus.FocusRing Name
    }

MicrolensTH.makeLenses '' TodoState

data AppState = AppState 
    { _screenFocusRing :: BFocus.FocusRing ScreenName
    , _todoState       :: TodoState
    }

MicrolensTH.makeLenses '' AppState

getScreen :: AppState -> ScreenName
getScreen s 
    = Maybe.fromJust 
    $ BFocus.focusGetCurrent 
    $ s ^. screenFocusRing

data ScreenData = ScreenData
    { draw
        :: AppState
        -> [BTypes.Widget Name]
    , chooseCursor
        :: AppState
        -> [BTypes.CursorLocation Name]
        -> Maybe (BTypes.CursorLocation Name)
    , handleEvent
        :: AppState
        -> BTypes.BrickEvent Name AppEvent
        -> BTypes.EventM Name (BTypes.Next AppState)
    }

defaultHorizontalLimit = 72
defaultVerticalLimit   = 32

screenMap :: Map ScreenName ScreenData
screenMap = Map.fromList
    [ ( TodoScreen
      , ScreenData
            { draw         = todoListDraw
            , chooseCursor = todoListChooseCursor
            , handleEvent  = todoListHandleEvent
            }
      )
    ]

defAttrMap :: AppState -> BAttr.AttrMap
defAttrMap _ = BAttr.attrMap VtyAttr.defAttr 
    [ ( BWList.listSelectedFocusedAttr
      , defaultStandout
      )
    ]
  where
    defaultStandout = VtyAttr.withStyle VtyAttr.defAttr VtyAttr.standout

defaultHandleEvent 
    :: AppState 
    -> BTypes.BrickEvent Name AppEvent 
    -> BTypes.EventM Name (BTypes.Next AppState)
defaultHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar 'c') [VtyEvents.MCtrl] -> BMain.halt s
    e -> handleEvent (screenMap ! getScreen s) s (BTypes.VtyEvent e)
defaultHandleEvent s e = handleEvent (screenMap ! getScreen s) s e

app :: BMain.App AppState AppEvent Name
app = BMain.App
    { BMain.appDraw         = (\s -> draw (getScreenData s) s)
    , BMain.appChooseCursor = (\s -> chooseCursor (getScreenData s) s)
    , BMain.appHandleEvent  = defaultHandleEvent
    , BMain.appStartEvent   = return
    , BMain.appAttrMap      = defAttrMap
    }
  where
    getScreenData s = screenMap ! getScreen s

ui :: IO ()
ui = void $ do
    json <- ByteString.readFile "todos"

    finalState <- BMain.defaultMain app AppState 
        { _screenFocusRing = BFocus.focusRing
            [ TodoScreen
            ]
        , _todoState = TodoState
            { _todoList = BWList.list 
                TodoList
                (Maybe.maybe [] id $ Aeson.decode json)
                1
            , _todoListFocusRing = BFocus.focusRing [TodoList]
            }
        }

    ByteString.writeFile "todos" 
        $ Aeson.encode 
        $ BWList.listElements 
        $ finalState ^. todoState . todoList

-- todo list stuff -------------------------------------------------------------

todoListDraw :: AppState -> [BTypes.Widget Name]
todoListDraw s = 
    [ BWCentre.center
    $ BWCore.withBorderStyle BWBStyle.unicodeBold
    $ BWBorder.borderWithLabel (BWCore.str "To-do Notation")
    $ BWCore.padTopBottom 1 
    $ BWCore.padLeftRight 3 
    $ BWCore.hLimit defaultHorizontalLimit 
    $ BWCore.vLimit defaultVerticalLimit 
    $ BWCore.vBox $
        [ BWCore.padBottom (BTypes.Pad 1) $ BWList.renderList 
            (\_ -> BWCore.str . show)
            True
            (s ^. todoState . todoList)
        ]
    ]

todoListChooseCursor 
    :: AppState 
    -> [BTypes.CursorLocation Name]
    -> Maybe (BTypes.CursorLocation Name)
todoListChooseCursor _ _ = Nothing

todoListHandleEvent
    :: AppState
    -> BTypes.BrickEvent Name AppEvent
    -> BTypes.EventM Name (BTypes.Next AppState)
todoListHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar ' ') [] -> BMain.continue
        $ s 
        & todoState . todoList 
        %~ BWList.listModify 
        (done %~ not)
    e -> do 
        newTodoList <- BWList.handleListEvent e (s ^. todoState . todoList)
        BMain.continue 
            $ s 
            & todoState . todoList 
            .~ newTodoList

todoListHandleEvent s _ = BMain.continue s