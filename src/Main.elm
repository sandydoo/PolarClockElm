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
  , delta : Float
  , clockArms : List ClockArm
  , width : Int
  , height : Int
  }


type alias ClockArm =
  { interval : List String
  , length : Int
  , fromTime : Time.Zone -> Time.Posix -> Int
  , radius : Float
  , armRadius : Float
  , angle : Anim.Animation
  }


init : Flags -> (Model, Cmd Msg)
init { currentTime, width, height } =
  ( { time = Time.millisToPosix currentTime
    , timezone = Time.utc
    , clockArms = createClockArms width height
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


createClockArms : Int -> Int -> List ClockArm
createClockArms width height =
  let
    clockRadius = (toFloat (min width height)) / 2
    armRadius = clockRadius / 22

    months = Calendar.monthsRange
    weekdays = Calendar.weekdaysRange
    days = Calendar.daysRange
    hours = Calendar.hoursRange
    minutes = Calendar.minutesRange
    seconds = Calendar.secondsRange
  in
  [ { interval = months
    , length = List.length months
    , fromTime = Calendar.toMonth
    , radius = 0.2 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { interval = weekdays
    , length = List.length weekdays
    , fromTime = Calendar.toWeekday
    , radius = 0.31 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { interval = days
    , length = List.length days
    , fromTime = Calendar.toDay
    , radius = 0.42 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { interval = hours
    , length = List.length hours
    , fromTime = Time.toHour
    , radius = 0.6 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { interval = minutes
    , length = List.length minutes
    , fromTime = Time.toMinute
    , radius = 0.71 * clockRadius
    , armRadius = armRadius
    , angle = Anim.static 0
    }
  , { interval = seconds
    , length = List.length seconds
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

        newClockArms =
          List.map updateAngle model.clockArms
      in
      ( { model | time = newTime, clockArms = newClockArms }
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
