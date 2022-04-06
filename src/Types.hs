{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Types where



-- imports ---------------------------------------------------------------------

-- language features
import GHC.Generics ( Generic )

-- data types
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- brick
import qualified Brick.Types        as BTypes
import qualified Brick.Widgets.List as BWList
import qualified Brick.Widgets.Edit as BWEdit
import qualified Brick.Forms        as BForms
import qualified Brick.Focus        as BFocus
import qualified Brick.AttrMap      as BAttr

-- microlens
import Lens.Micro
    ( (&) -- flipped $
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )
import qualified Lens.Micro    as Microlens
import qualified Lens.Micro.TH as MicrolensTH

-- aeson
import qualified Data.Aeson as Aeson

-- misc
import qualified Data.Maybe as Maybe



-- new data types --------------------------------------------------------------

-- name types

type AppEvent = ()

data Name
    = None
    | CommandPrompt
    | TodoList
    | HabitList
    deriving (Eq, Ord, Show)

data ScreenName
    = TodoScreen
    | HabitScreen
    | ScheduleScreen
    | TimelineScreen
    deriving (Eq, Ord, Show)



-- todo stuff

data Priority
    = NoPriority
    | LowPriority
    | MediumPriority
    | HighPriority
    | UrgentPriority
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Aeson.ToJSON Priority where
instance Aeson.FromJSON Priority where

data Todo = Todo
    { _todoName     :: Text
    , _todoDone     :: Bool
    , _todoPriority :: Priority
    } deriving (Eq, Generic)

MicrolensTH.makeLenses '' Todo

instance Aeson.ToJSON Todo where
instance Aeson.FromJSON Todo where

instance Show Todo where
    show t
        = '[' : (if t ^. todoDone then '/' else ' ') : ']' : ' '
        : Text.unpack (t ^. todoName)
        -- ++ " " ++ show (t ^. todoPriority)



-- app state

data TodoState = TodoState
    { _todoList      :: BWList.GenericList Name Seq Todo
    , _todoFocusRing :: BFocus.FocusRing Name
    }

MicrolensTH.makeLenses '' TodoState

data AppState = AppState
    { _debug                :: String
    , _widgetFocusRing      :: BFocus.FocusRing Name
    , _screenFocusRing      :: BFocus.FocusRing ScreenName
    , _commandPrompt        :: BWEdit.Editor Text Name
    , _previousCommands     :: Seq Text
    , _previousCommandIndex :: Int
    , _errorMessage         :: Text
    , _todoState            :: TodoState
    }

MicrolensTH.makeLenses '' AppState

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
    , handleCommand
        :: AppState 
        -> Command
        -> AppState
    , focusRing :: BFocus.FocusRing Name
    }



-- commands

data CommandName
    = QuitCommandName
    | HelpCommandName
    | NewTodoCommandName
    | MarkTodoCommandName

data Command
    = QuitCommand
    | HelpCommand
    | NewTodoCommand
        { newTodoName     :: Text
        , newTodoPriority :: Priority
        } 
    | MarkTodoCommand
        { markTodoName :: Text
        , markTodoDone :: Bool
        } 
    deriving (Eq, Show)



-- utility functions -----------------------------------------------------------

getFocusUnsafe :: BFocus.FocusRing n -> n
getFocusUnsafe = Maybe.fromJust . BFocus.focusGetCurrent

getScreen :: AppState -> ScreenName
getScreen s = getFocusUnsafe $ s ^. screenFocusRing

getWidgetFocus :: AppState -> Name
getWidgetFocus s = getFocusUnsafe $ s ^. widgetFocusRing



-- attribute names -------------------------------------------------------------

urgentPriorityAttr :: BAttr.AttrName
urgentPriorityAttr = BAttr.attrName "urgent"

highPriorityAttr :: BAttr.AttrName
highPriorityAttr = BAttr.attrName "high"

mediumPriorityAttr :: BAttr.AttrName
mediumPriorityAttr = BAttr.attrName "medium"

lowPriorityAttr :: BAttr.AttrName
lowPriorityAttr = BAttr.attrName "low"
