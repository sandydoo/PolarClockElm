module Main exposing (..)


import Animation as Anim
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Ease exposing (outQuart)
import Json.Decode exposing (Decoder, field, int, map3)
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


type alias Model =
  { time : Time.Posix
  , timezone : Time.Zone
  , date : Calendar.Date
  , delta : Float
  , clockArms : List ClockArm
  , width : Int
  , height : Int
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
init { currentTime, width, height } =
  let
    time = Time.millisToPosix currentTime
    timezone = Time.utc
    date = Calendar.fromPosix timezone time
    datetime = { date = date, time = time }
  in
  ( { time = time
    , timezone = timezone
    , date = date
    , clockArms = createClockArms width height datetime
    , width = width
    , height = height
    , delta = 0
    }
  , Task.perform UpdateTimeZone Time.here
  )


type alias Flags =
  { currentTime : Int
  , width : Int
  , height : Int
  }


flagsDecoder : Decoder Flags
flagsDecoder =
  map3 Flags
    (field "currentTime" int)
    (field "width" int)
    (field "height" int)


createClockArms : Int -> Int -> Calendar.DateTime -> List ClockArm
createClockArms width height datetime =
  let
    clockRadius = (toFloat (min width height)) / 2
    armRadius = clockRadius / 22

    calendar = Calendar.createDateRanges datetime
  in
  [ { range = calendar.months
    , length = List.length calendar.months
    , updateRange = Nothing
    , fromTime = Calendar.toMonth
    , radius = 0.2 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.weekdays
    , length = List.length calendar.weekdays
    , updateRange = Nothing
    , fromTime = Calendar.toWeekday
    , radius = 0.31 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.days
    , length = List.length calendar.days
    , updateRange = Just Calendar.daysRange
    , fromTime = Calendar.toDay
    , radius = 0.42 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.hours
    , length = List.length calendar.hours
    , updateRange = Nothing
    , fromTime = Time.toHour
    , radius = 0.6 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.minutes
    , length = List.length calendar.minutes
    , updateRange = Nothing
    , fromTime = Time.toMinute
    , radius = 0.71 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { range = calendar.seconds
    , length = List.length calendar.seconds
    , updateRange = Nothing
    , fromTime = Time.toSecond
    , radius = 0.82 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  ]



-- Update


type Msg
  = UpdateTime Time.Posix
  | UpdateTimeZone Time.Zone
  | Animate Float


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

        newDatetime = { time = newTime, date = newDate }

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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 UpdateTime
    , onAnimationFrameDelta Animate
    ]



-- View


view : Model -> Html Msg
view model =
  Html.div [] [ Draw.drawClock model ]
