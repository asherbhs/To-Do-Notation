{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Types where



-- imports ---------------------------------------------------------------------

-- language features
import GHC.Generics (Generic)

-- internal
import qualified Util

-- data types
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Sequence (Seq)

import Data.Set (Set)

-- brick
import qualified Brick.Types        as BTypes
import qualified Brick.Widgets.List as BWList
import qualified Brick.Widgets.Edit as BWEdit
import qualified Brick.Focus        as BFocus
import qualified Brick.AttrMap      as BAttr

-- microlens
import Lens.Micro ((^.))
import qualified Lens.Micro.TH as MicrolensTH

-- aeson
import qualified Data.Aeson as Aeson

-- time
import qualified Data.Time as Time

-- misc
import qualified Data.Maybe as Maybe



-- instances -------------------------------------------------------------------
instance BWList.Splittable [] where
    splitAt n xs = (take n xs, drop n xs)

-- new data types --------------------------------------------------------------

-- name types

type AppEvent = ()

data Name
    = None
    | CommandPrompt
    | TodoList
    | HabitLists
    | HabitList Int
    deriving (Eq, Ord, Show)

data ScreenName
    = TodoScreen
    | HabitScreen
    | ScheduleScreen
    | TimelineScreen
    deriving (Eq, Ord, Show)



-- todo stuff

{-
priorities are just ints, with larger meaning a higher priority

five aliases are defined and are given special rendering in other parts of the
code, but the user can provide an arbitrary int and it can still be stored
-}
type Priority = Int

pattern UrgentPriority :: Priority
pattern UrgentPriority = 4

pattern HighPriority :: Priority
pattern HighPriority = 3

pattern MediumPriority :: Priority
pattern MediumPriority = 2

pattern LowPriority :: Priority
pattern LowPriority = 1

pattern NoPriority :: Priority
pattern NoPriority = 0

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

data TodoState = TodoState
    { _todoList      :: BWList.GenericList Name Seq Todo
    , _todoFocusRing :: BFocus.FocusRing Name
    }

MicrolensTH.makeLenses '' TodoState



-- habit stuff
data HabitRepeat
    = EveryDay
    | EveryOtherDay
    | EveryWeekday
    | EveryMonday
    | EveryTuesday
    | EveryWednesday
    | EveryThursday
    | EveryFriday
    | EverySaturday
    | EverySunday
    deriving (Eq, Show, Generic)

instance Aeson.ToJSON HabitRepeat where
instance Aeson.FromJSON HabitRepeat where

data HabitDay = HabitDay
    { _habitDay  :: Time.Day
    , _habitDue  :: Bool
    , _habitDone :: Bool
    }

MicrolensTH.makeLenses '' HabitDay

instance Show HabitDay where
    show (HabitDay day due done)
        = Util.padLeft 12 ' ' $ show d ++ "/" ++ show m ++ " " ++
            [ head $ show $ Time.dayOfWeek day
            , ' '
            , if due then '[' else ' '
            , if done then '*' else ' '
            , if due then ']' else ' '
            , ' '
            ]
      where
        (_, m, d) = Time.toGregorian day


data Habit = Habit
    { _habitName      :: Text
    , _habitStart     :: Time.Day
    , _habitRepeat    :: [HabitRepeat]
    , _habitCompleted :: Set Time.Day
    } deriving (Eq, Show, Generic)

MicrolensTH.makeLenses '' Habit

instance Aeson.ToJSON Habit where
instance Aeson.FromJSON Habit where

data HabitState = HabitState
    { _habitList  :: [(Habit, BWList.GenericList Name [] HabitDay)]
    , _habitFocus :: Int
    }

MicrolensTH.makeLenses '' HabitState



-- app state

data AppState = AppState
    { _debug                :: String
    , _widgetFocusRing      :: BFocus.FocusRing Name
    , _screenFocusRing      :: BFocus.FocusRing ScreenName
    , _commandPrompt        :: BWEdit.Editor Text Name
    , _previousCommands     :: Seq Text
    , _previousCommandIndex :: Int
    , _errorMessage         :: Text
    , _today                :: Time.Day
    , _todoState            :: TodoState
    , _habitState           :: HabitState
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
    | NewHabitCommandName

data Command
    = QuitCommand
    | HelpCommand
    | NewTodoCommand
        { newTodoName     :: Text
        , newTodoPriority :: Priority
        }
    | NewHabitCommand
        { newHabitName   :: Text
        , newHabitRepeat :: [HabitRepeat]
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

extraPriorityAttr :: BAttr.AttrName
extraPriorityAttr = BAttr.attrName "extra"

urgentPriorityAttr :: BAttr.AttrName
urgentPriorityAttr = BAttr.attrName "urgent"

highPriorityAttr :: BAttr.AttrName
highPriorityAttr = BAttr.attrName "high"

mediumPriorityAttr :: BAttr.AttrName
mediumPriorityAttr = BAttr.attrName "medium"

lowPriorityAttr :: BAttr.AttrName
lowPriorityAttr = BAttr.attrName "low"
