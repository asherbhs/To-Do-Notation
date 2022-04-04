module UIHelp where

-- internal
import qualified Types

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


-- microlens
import Lens.Micro
    ( (&) -- flipped $
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )
import qualified Lens.Micro    as Microlens
import qualified Lens.Micro.TH as MicrolensTH

screenBox
    :: Types.AppState
    -> [BTypes.Widget Types.Name]
    -> BTypes.Widget Types.Name
screenBox s = BWCentre.center
    . BWCore.withBorderStyle BWBStyle.unicodeBold
    . BWBorder.borderWithLabel
        (BWCore.str $ " " ++ s ^. Types.debug ++ " ")
    . BWCore.padTopBottom 1
    . BWCore.padLeftRight 3
    -- . BWCore.hLimit 72
    -- . BWCore.vLimit 32
    . BWCore.vBox
  where
    screenLabel = case Types.getScreen s of
        Types.TodoScreen  -> "To-do Notation"
        Types.HabitScreen -> "Habit Tracker"
        _           -> "..."