module Main exposing (..)


import Animation as Anim
import Browser
import Browser.Events as Event
import Ease exposing (outQuart)
import Json.Decode exposing (Decoder, field, int, map2)
import Html exposing (Html)
import Html.Attributes
import Task
import Time

import Draw
import Calendar



-- Main

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- Model


type alias Dimensions =
  { width : Int
  , height : Int
  }


type alias Model =
  { time : Time.Posix
  , timezone : Time.Zone
  , date : Calendar.Date
  , delta : Float
  , clockArms : List ClockArm
  , dimensions : Dimensions
  }


type alias ClockArm =
  { range : List String
  , length : Int
  , updateRange : Maybe (Calendar.DateTime -> List String)
  , fromTime : Time.Zone -> Time.Posix -> Int
  , radius : Float
  , armRadius : Float
  , angle : Anim.Animation
  }


init : Flags -> (Model, Cmd Msg)
init { currentTime, dimensions } =
  let
    time = Time.millisToPosix currentTime
    timezone = Time.utc
    date = Calendar.fromPosix timezone time
    datetime = { date = date, time = time }
    clockArms = createClockArms datetime
  in
  ( { time = time
    , timezone = timezone
    , date = date
    , clockArms = clockArms
    , dimensions = dimensions
    , delta = 0
    }
  , Task.perform UpdateTimeZone Time.here
  )


type alias Flags =
  { currentTime : Int
  , dimensions: Dimensions
  }


flagsDecoder : Decoder Flags
flagsDecoder =
  let
    dimensionsDecoder : Decoder Dimensions
    dimensionsDecoder =
      map2 Dimensions
        (field "width" int)
        (field "height" int)
  in
  map2 Flags
    (field "currentTime" int)
    (field "dimensions" dimensionsDecoder)


createClockArms : Calendar.DateTime -> List ClockArm
createClockArms datetime =
  let
    calendar = Calendar.createDateRanges datetime
    armRadius = 23
  in
  [ { range = calendar.months
    , length = List.length calendar.months
    , updateRange = Nothing
    , fromTime = Calendar.toMonth
    , radius = 100
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.weekdays
    , length = List.length calendar.weekdays
    , updateRange = Nothing
    , fromTime = Calendar.toWeekday
    , radius = 155
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.days
    , length = List.length calendar.days
    , updateRange = Just Calendar.daysRange
    , fromTime = Calendar.toDay
    , radius = 210
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.hours
    , length = List.length calendar.hours
    , updateRange = Nothing
    , fromTime = Time.toHour
    , radius = 300
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.minutes
    , length = List.length calendar.minutes
    , updateRange = Nothing
    , fromTime = Time.toMinute
    , radius = 355
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.seconds
    , length = List.length calendar.seconds
    , updateRange = Nothing
    , fromTime = Time.toSecond
    , radius = 410
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  ]



-- Update


type Msg
  = UpdateTime Time.Posix
  | UpdateTimeZone Time.Zone
  | Animate Float
  | Resize Dimensions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateTimeZone newTimezone ->
      ( { model | timezone = newTimezone }
      , Cmd.none
      )

    UpdateTime newTime ->
      let
        newDate = Calendar.fromPosix model.timezone newTime

        newAnimatedAngle { angle, length, fromTime } =
          let
            newAngle =
              toFloat (fromTime model.timezone newTime) / toFloat length * 360
          in
          Anim.retarget model.delta newAngle angle
            |> Anim.duration 750
            |> Anim.ease outQuart

        updateAngle clockArm =
          { clockArm | angle = newAnimatedAngle clockArm }

        newDatetime = { date = newDate, time = newTime }

        updateDates datetime clockArm =
          case clockArm.updateRange of
            Nothing -> clockArm

            Just updateRange ->
              let
                newRange = updateRange datetime
                newLength = List.length newRange
              in
              { clockArm | range = newRange, length = newLength }

        newClockArms =
          List.map (updateDates newDatetime >> updateAngle) model.clockArms
      in
      ( { model | time = newTime, date = newDate, clockArms = newClockArms }
      , Cmd.none
      )

    Animate newDelta ->
      ( { model | delta = model.delta + newDelta }
      , Cmd.none
      )

    Resize newDimensions ->
      ( { model | dimensions = newDimensions }
      , Cmd.none)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 UpdateTime
    , Event.onAnimationFrameDelta Animate
    , Event.onResize (\width height -> Resize (Dimensions width height))
    ]



-- View


view : Model -> Html Msg
view model =
  Html.div [] [ Draw.drawClock model ]
