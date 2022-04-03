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
import qualified Brick.Forms        as BForms
import qualified Brick.Focus        as BFocus

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

type AppEvent = ()

data Name
    = None
    | TodoList
    | TodoForm
    deriving (Eq, Ord, Show)

data ScreenName
    = TodoScreen
    | HabitScreen
    | ScheduleScreen
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
        : Text.unpack (t ^. name)

data TodoState = TodoState
    { _todoList      :: BWList.GenericList Name Seq Todo
    , _todoForm      :: BForms.Form Todo () Name
    , _todoFocusRing :: BFocus.FocusRing Name
    }

MicrolensTH.makeLenses '' TodoState

data AppState = AppState
    { _debug           :: String
    , _screenFocusRing :: BFocus.FocusRing ScreenName
    , _todoState       :: TodoState
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
    }

-- utility functions -----------------------------------------------------------

getFocusUnsafe :: BFocus.FocusRing n -> n
getFocusUnsafe = Maybe.fromJust . BFocus.focusGetCurrent

getScreen :: AppState -> ScreenName
getScreen s = getFocusUnsafe $ s ^. screenFocusRing

getTodoFocus :: AppState -> Name
getTodoFocus s = getFocusUnsafe $ s ^. todoState . todoFocusRing
