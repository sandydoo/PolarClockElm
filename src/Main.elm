module Main exposing (..)


import Animation as Anim
import Browser
import Browser.Events as Event
import Json.Decode exposing ( Decoder, field, int, map2 )
import Html exposing ( Html )
import Html.Attributes
import Task
import Time

import Calendar exposing ( DateTime(..) )
import Clock
import Draw
import Window



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
  { datetime   : Calendar.DateTime
  , clockArms  : List Clock.Arm
  , dimensions : Window.Dimensions
  , delta      : Float
  }



-- Flags


type alias Flags =
  { currentTime : Int
  , dimensions  : Window.Dimensions
  }


flagsDecoder : Decoder Flags
flagsDecoder =
  map2 Flags
    ( field "currentTime" int )
    ( field "dimensions" <|
        map2 Window.Dimensions
          ( field "width"  int )
          ( field "height" int )
    )



init : Flags -> ( Model, Cmd Msg )
init { currentTime, dimensions } =
  let
    time = Time.millisToPosix currentTime

    zone = Time.utc

    datetime = Calendar.toDatetime zone time

    clockArms = Clock.init datetime
  in
  ( { datetime   = datetime
    , clockArms  = clockArms
    , dimensions = dimensions
    , delta      = 0
    }
  , Task.perform UpdateTimeZone Time.here
  )



-- Update


type Msg
  = UpdateTime Time.Posix
  | UpdateTimeZone Time.Zone
  | Animate Float
  | Resize Window.Dimensions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateTimeZone newZone ->
      let
        ( DateTime _ _ time ) = model.datetime
        newDatetime = Calendar.toDatetime newZone time
      in
      ( { model | datetime = newDatetime }
      , Cmd.none
      )

    UpdateTime newTime ->
      let
        ( DateTime _ zone _ ) = model.datetime

        newDatetime = Calendar.toDatetime zone newTime

        newClockArms = Clock.update newDatetime model.delta model.clockArms
      in
      ( { model | datetime = newDatetime, clockArms = newClockArms }
      , Cmd.none
      )

    Animate newDelta ->
      ( { model | delta = model.delta + newDelta }
      , Cmd.none
      )

    Resize newDimensions ->
      ( { model | dimensions = newDimensions }
      , Cmd.none
      )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 UpdateTime
    , Event.onAnimationFrameDelta Animate
    , Event.onResize
        ( \width height -> Resize { width = width, height = height } )
    ]



-- View


view : Model -> Html Msg
view model =
  let
    { dimensions, clockArms, delta } = model
  in
  Html.div [] [ Draw.clock dimensions clockArms delta ]
