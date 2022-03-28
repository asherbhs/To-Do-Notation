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

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- ui
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

type AppEvent = ()

data Name
    = None
    | TodoList
    | TodoForm
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

data TodoState = TodoState
    { _todoList      :: BWList.GenericList Name Seq Todo
    , _todoForm      :: BForms.Form Todo () Name
    , _todoFocusRing :: BFocus.FocusRing Name
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

getTodoFocus :: AppState -> Name
getTodoFocus s 
    = Maybe.fromJust
    $ BFocus.focusGetCurrent 
    $ s ^. todoState . todoFocusRing

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
            { draw         = todoDraw
            , chooseCursor = todoChooseCursor
            , handleEvent  = todoHandleEvent
            }
      )
    ]

emptyTodoForm :: BForms.Form Todo () Name
emptyTodoForm = BForms.newForm
    [(BWCore.str ">>= " <+>) @@= BForms.editTextField name TodoForm (Just 1)]
    $ Todo "" False

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
                ( Maybe.maybe 
                    Seq.empty 
                    id 
                    $ Aeson.decode json
                )
                1
            , _todoForm = emptyTodoForm
            , _todoFocusRing = BFocus.focusRing [TodoList, TodoForm]
            }
        }

    ByteString.writeFile "todos" 
        $ Aeson.encode 
        $ BWList.listElements 
        $ finalState ^. todoState . todoList

-- todo list stuff -------------------------------------------------------------

todoDraw :: AppState -> [BTypes.Widget Name]
todoDraw s = 
    [ BWCentre.center
    $ BWCore.withBorderStyle BWBStyle.unicodeBold
    $ BWBorder.borderWithLabel (BWCore.str "To-do Notation")
    $ BWCore.padTopBottom 1
    $ BWCore.padLeftRight 3
    $ BWCore.hLimit defaultHorizontalLimit
    $ BWCore.vLimit defaultVerticalLimit
    $ BWCore.vBox $
        [ BWList.renderList
            (const $ BWCore.str . show) 
            (getTodoFocus s == TodoList)
            (s ^. todoState . todoList)

        , BWCore.padTopBottom 1 
        $ BWCore.withBorderStyle BWBStyle.unicode
        $ BWBorder.hBorderWithLabel (BWCore.str "New To-do")

        , BForms.renderForm $ s ^. todoState . todoForm
        ]
    ]

todoChooseCursor 
    :: AppState 
    -> [BTypes.CursorLocation Name]
    -> Maybe (BTypes.CursorLocation Name)
todoChooseCursor s [l] = case l ^. BTypes.cursorLocationNameL of
    Just TodoForm -> if getTodoFocus s == TodoForm then Just l else Nothing
    _ -> Nothing
todoChooseCursor _ _   = Nothing

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

    VtyEvents.EvKey (VtyEvents.KBS) [] -> BMain.continue
        $ s
        & todoState . todoList
        %~ \l -> case BWList.listSelected l of
            Just i  -> BWList.listRemove i l
            Nothing -> l

    e -> do 
        newTodoList <- BWList.handleListEvent e (s ^. todoState . todoList)
        BMain.continue 
            $ s 
            & todoState . todoList 
            .~ newTodoList

todoListHandleEvent s _ = BMain.continue s

todoFormHandleEvent
    :: AppState
    -> BTypes.BrickEvent Name AppEvent
    -> BTypes.EventM Name (BTypes.Next AppState)
todoFormHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KEnter) [] -> BMain.continue
        $ case newTodo ^. name of
            "" -> s
            _  -> s 
                & todoState . todoList
                %~ 
                ( \l -> BWList.listMoveToEnd $ BWList.listInsert
                    (length $ BWList.listElements l)
                    newTodo
                    l
                )

                & todoState . todoForm
                .~ emptyTodoForm

    e -> do
        newForm <- BForms.handleFormEvent 
            (BTypes.VtyEvent e)
            (s ^. todoState . todoForm)
        BMain.continue 
            $ s
            & todoState . todoForm
            .~ newForm
  where
    newTodo = BForms.formState $ s ^. todoState . todoForm

todoFormHandleEvent s _ = BMain.continue s

todoHandleEvent
    :: AppState
    -> BTypes.BrickEvent Name AppEvent
    -> BTypes.EventM Name (BTypes.Next AppState)
todoHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar '\t') [] -> BMain.continue
        $ s
        & todoState . todoFocusRing
        %~ BFocus.focusNext

    e -> case getTodoFocus s of
        TodoList -> todoListHandleEvent s (BTypes.VtyEvent e)
        TodoForm -> todoFormHandleEvent s (BTypes.VtyEvent e)

todoHandleEvent s _ = BMain.continue s