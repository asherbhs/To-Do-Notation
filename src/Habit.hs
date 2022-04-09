module Habit where

import qualified Types
import qualified UIHelp


-- ui
import qualified Brick.Types         as BTypes
import qualified Brick.Main          as BMain
import qualified Brick.Focus         as BFocus
import Brick.Widgets.Core ((<=>))
import qualified Brick.Widgets.Core  as BWCore
import qualified Brick.Widgets.List  as BWList

import qualified Graphics.Vty.Input.Events as VtyEvents

-- microlens
import Lens.Micro
    ( (&) -- flipped $
    , (^.) -- view
    , (%~) -- over
    , (.~) -- set
    )
import qualified Lens.Micro as Lens

-- time
import Data.Time as Time

-- set
import qualified Data.Set as Set

-- misc
import Data.List (intersperse)
import Data.List.Index (imap, setAt, modifyAt)
import qualified Brick.Widgets.List as BWlist
import qualified Data.Maybe as Maybe

--------------------------------------------------------------------------------

draw :: Types.AppState -> [BTypes.Widget Types.Name]
draw s =
    [ UIHelp.screenBox s
        [ BWCore.hBox $ intersperse gap (imap
                (\i (h, l) ->
                    BWCore.txt (h ^. Types.habitName)
                    <=>
                    BWCore.hLimit 12 (BWList.renderList
                        (\_ -> BWCore.str . show)
                        (i == s ^. Types.habitState . Types.habitFocus
                            && Types.getWidgetFocus s == Types.HabitLists)
                        l
                    )
                )
                (s ^. Types.habitState . Types.habitList))
            ++ [gap]
        ]
    ]
  where
    gap = BWCore.fill ' '

chooseCursor
     :: Types.AppState
    -> [BTypes.CursorLocation Types.Name]
    -> Maybe (BTypes.CursorLocation Types.Name)
chooseCursor _ _ = Nothing

handleEvent
    :: Types.AppState
    -> BTypes.BrickEvent Types.Name Types.AppEvent
    -> BTypes.EventM Types.Name (BTypes.Next Types.AppState)
handleEvent s (BTypes.VtyEvent e) = case e of
    VtyEvents.EvKey (VtyEvents.KChar ' ') [] -> BMain.continue $ s 
    -- hacked together indexing solution until I can figure out the lenses bug
    -- seriously this is disgusting but it works for now
        & Types.habitState . Types.habitList %~ (\l ->
            let (h, oldHabitDays) = l !! (s ^. Types.habitState . Types.habitFocus) in
                setAt
                    (s ^. Types.habitState . Types.habitFocus)
                    ( h & Types.habitCompleted %~ (\s -> let d = (snd $ Maybe.fromJust $ BWList.listSelectedElement oldHabitDays) ^. Types.habitDay in
                            if Set.member d s
                            then Set.delete d s
                            else Set.insert d s
                        )
                    , oldHabitDays & BWlist.listElementsL %~ modifyAt 
                        (Maybe.fromJust $ oldHabitDays ^. BWList.listSelectedL) 
                        (Types.habitDone %~ not)
                    )
                    l
        )

    VtyEvents.EvKey VtyEvents.KLeft [] -> BMain.continue $ s
        & Types.habitState . Types.habitFocus
        %~ \i -> max (i - 1) 0

    VtyEvents.EvKey VtyEvents.KRight [] -> BMain.continue $ s
        & Types.habitState . Types.habitFocus
        %~ \i -> min (i + 1) (length (s ^. Types.habitState . Types.habitList) - 1)

    _ -> do
        -- temporary solution with indexing until I can figure out that lens bug
        newHabitDays <- BWList.handleListEventVi
            BWList.handleListEvent
            e
            (snd $ (s ^. Types.habitState . Types.habitList) !! (s ^. Types.habitState . Types.habitFocus))
        BMain.continue $ s & Types.habitState . Types.habitList %~ (\l ->
                let (h, _) = l !! (s ^. Types.habitState . Types.habitFocus) in
                    setAt (s ^. Types.habitState . Types.habitFocus) (h, newHabitDays) l
            )
        -- do
        -- newHabitDays <- BWList.handleListEvent 
        --     e 
        --     (s ^. Types.habitState
        --         . Types.habitList
        --         . Lens.ix (s ^. Types.habitState . Types.habitFocus)
        --         . Lens._2)
        -- BMain.continue $ s & Types.habitState
        --     . Types.habitList
        --     . Lens.ix (s ^. Types.habitState . Types.habitFocus)
        --     . Lens._2 .~ newHabitDays
    --   where 
    --     focusedListLens 
    --         = Types.habitState
    --         . Types.habitList
    --         . Lens.ix (s ^. Types.habitState . Types.habitFocus)
    --         . Lens._2

handleEvent s _ = BMain.continue s

habitDaysList 
    :: Types.Habit
    -> Day
    -> Int
    -> BWlist.GenericList Types.Name [] Types.HabitDay
habitDaysList h today i = BWList.list
    (Types.HabitList i)
    (map
        (\d -> Types.HabitDay d (isHabitDue d) (Set.member d $ h ^. Types.habitCompleted))
        [today, Time.addDays (-1) today ..])
    1
  where
    isHabitDue :: Time.Day  -> Bool
    isHabitDue day = any (isHabitDueSingle day) (h ^. Types.habitRepeat)

    isHabitDueSingle :: Time.Day -> Types.HabitRepeat -> Bool
    isHabitDueSingle _ Types.EveryDay       = True
    isHabitDueSingle d Types.EveryOtherDay  = even $ Time.diffDays today d
    isHabitDueSingle d Types.EveryWeekday
        = Time.dayOfWeek d `elem` [Time.Monday .. Time.Friday]
    isHabitDueSingle d Types.EveryMonday    = Time.dayOfWeek d == Time.Monday
    isHabitDueSingle d Types.EveryTuesday   = Time.dayOfWeek d == Time.Tuesday
    isHabitDueSingle d Types.EveryWednesday = Time.dayOfWeek d == Time.Wednesday
    isHabitDueSingle d Types.EveryThursday  = Time.dayOfWeek d == Time.Thursday
    isHabitDueSingle d Types.EveryFriday    = Time.dayOfWeek d == Time.Friday
    isHabitDueSingle d Types.EverySaturday  = Time.dayOfWeek d == Time.Saturday
    isHabitDueSingle d Types.EverySunday    = Time.dayOfWeek d == Time.Sunday

handleCommand
    :: Types.AppState
    -> Types.Command
    -> Types.AppState
handleCommand s (Types.NewHabitCommand name repeats) = 
  let
    today :: Time.Day
    today = s ^. Types.today

    h = Types.Habit
        { Types._habitName      = name
        , Types._habitStart     = today
        , Types._habitRepeat    = repeats
        , Types._habitCompleted = Set.empty
        }
  in
    s & Types.habitState . Types.habitList %~ (++ [
            ( h
            , habitDaysList h today (length $ s ^. Types.habitState . Types.habitList)
            )
        ])

handleCommand s _ = s

focusRing :: BFocus.FocusRing Types.Name
focusRing = BFocus.focusRing [Types.HabitLists]
