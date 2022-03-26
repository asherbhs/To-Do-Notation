module UI where

import Brick.Main 
    ( App(..)
    , defaultMain
    )

import Data.Map (Map, (!))
import qualified Data.Map as Map

import AppData
import qualified TodoList

import Control.Monad (void)

--------------------------------------------------------------------------------

screenMap :: Map ScreenName ScreenData
screenMap = Map.fromList
    [ ( TodoListScreen
      , ScreenData
            { draw = TodoList.draw
            , chooseCursor = TodoList.chooseCursor
            , handleEvent = TodoList.handleEvent
            }
      )
    ]

app :: App AppState AppEvent ResourceName
app = App
    { appDraw         = (\s -> draw         (screenMap ! (screen s)) s)
    , appChooseCursor = (\s -> chooseCursor (screenMap ! (screen s)) s)
    , appHandleEvent  = (\s -> handleEvent  (screenMap ! (screen s)) s)
    , appStartEvent   = return 
    , appAttrMap      = defAttrMap
    }
  where
    getScreenData = (\s -> screenMap ! (screen s))

ui :: IO ()
ui = void $ do
    defaultMain app initialState
  where
    initialState = AppState TodoListScreen