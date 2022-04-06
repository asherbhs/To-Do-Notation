{-# LANGUAGE OverloadedStrings #-}

module UIHelp where

-- internal
import qualified Types

-- brick
import qualified Brick.Main    as BMain
import qualified Brick.Types   as BTypes
import qualified Brick.AttrMap as BAttr
import qualified Brick.Focus   as BFocus

import           Brick.Widgets.Core ((<+>), (<=>))
import qualified Brick.Widgets.Core         as BWCore
import qualified Brick.Widgets.Center       as BWCentre
import qualified Brick.Widgets.Border       as BWBorder
import qualified Brick.Widgets.Border.Style as BWBStyle
import qualified Brick.Widgets.List         as BWList
import qualified Brick.Widgets.Edit         as BWEdit

-- microlens
import Lens.Micro
    ( (&) -- flipped $
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )
import qualified Lens.Micro    as Microlens
import qualified Lens.Micro.TH as MicrolensTH

--------------------------------------------------------------------------------

approxFontRatio :: Double
approxFontRatio = 0.45

screenBox
    :: Types.AppState
    -> [BTypes.Widget Types.Name]
    -> BTypes.Widget Types.Name
screenBox s ws 
    = BWCentre.center
    $ BWCore.withBorderStyle BWBStyle.unicodeBold
    $ BWBorder.borderWithLabel (BWCore.str $ " " ++ screenLabel ++ " ") 
        -- screenLabel
        -- s ^. Types.debug
        -- show (s ^. Types.previousCommandIndex) ++ " " ++ show (s ^. Types.previousCommands)
    $ BWCore.padLeftRight 2
    $ BWCore.padTopBottom (round $ approxFontRatio * 2.0)
    $ BWCore.hLimit (round hlim)
    $ BWCore.vLimit (round $ approxFontRatio * hlim)
    $ BWCore.vBox
    $ ws ++ map (BWCore.padTop $ BTypes.Pad 1)
        [ BWCore.withBorderStyle BWBStyle.unicode BWBorder.hBorder
        , if errorMessage == "" then 
            BWCore.emptyWidget 
          else 
            BWCore.txt errorMessage
        , BWCore.str ">>= " <+> BWEdit.renderEditor
            (BWCore.txt . head)
            (Types.getWidgetFocus s == Types.CommandPrompt)
            (s ^. Types.commandPrompt) 
        ]
  where
    screenLabel = case Types.getScreen s of
        Types.TodoScreen  -> "To-do Notation"
        Types.HabitScreen -> "Habit Tracker"
        _           -> "..."
    errorMessage = s ^. Types.errorMessage
    hlim = 72.0