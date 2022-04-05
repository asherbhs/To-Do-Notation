{-# LANGUAGE OverloadedStrings #-}
module Todo where

-- imports ---------------------------------------------------------------------

-- internal
import qualified Types
import qualified UIHelp

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

emptyForm :: BForms.Form Types.Todo () Types.Name
emptyForm = BForms.newForm
    [ (BWCore.str ">>= " <+>) 
        @@= BForms.editTextField Types.todoName Types.TodoForm (Just 1)
    ]
    $ Types.Todo "" False 0

draw :: Types.AppState -> [BTypes.Widget Types.Name]
draw s =
    [ UIHelp.screenBox s
        [ BWList.renderList
            (const $ BWCore.str . show)
            (Types.getTodoFocus s == Types.TodoList)
            (s ^. Types.todoState . Types.todoList)

        , BWCore.padTopBottom 1
            $ BWCore.withBorderStyle BWBStyle.unicode
            $ BWBorder.hBorderWithLabel (BWCore.str "New To-do")

        , BForms.renderForm $ s ^. Types.todoState . Types.todoForm
        ]
    ]

chooseCursor
    :: Types.AppState
    -> [BTypes.CursorLocation Types.Name]
    -> Maybe (BTypes.CursorLocation Types.Name)
chooseCursor s [l] = case l ^. BTypes.cursorLocationNameL of
    Just Types.TodoForm -> if Types.getTodoFocus s == Types.TodoForm then Just l else Nothing
    _ -> Nothing
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

todoFormHandleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
todoFormHandleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey VtyEvents.KEnter [] -> BMain.continue $
        case newTodo ^. Types.todoName of
            "" -> s
            _  -> s
                & Types.todoState . Types.todoList
                %~
                ( \l -> BWList.listMoveToEnd $ BWList.listInsert
                    (length $ BWList.listElements l)
                    newTodo
                    l
                )

                & Types.todoState . Types.todoForm
                .~ emptyForm

    e -> do
        newForm <- BForms.handleFormEvent
            (BTypes.VtyEvent e)
            (s ^. Types.todoState . Types.todoForm)
        BMain.continue
            $ s
            & Types.todoState . Types.todoForm
            .~ newForm
  where
    newTodo = BForms.formState $ s ^. Types.todoState . Types.todoForm

todoFormHandleEvent s _ = BMain.continue s

handleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
handleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar '\t') [] -> BMain.continue
        $ s
        & Types.todoState . Types.todoFocusRing
        %~ BFocus.focusNext

    e -> case Types.getTodoFocus s of
        Types.TodoList -> todoListHandleEvent s (BTypes.VtyEvent e)
        Types.TodoForm -> todoFormHandleEvent s (BTypes.VtyEvent e)
        _              -> BMain.continue s

handleEvent s _ = BMain.continue s