module Main exposing (..)


import Animation as Anim
import Browser
import Browser.Events as Event
import Json.Decode as Decode exposing ( Decoder )
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
  { datetime        : Calendar.DateTime
  , clockArms       : List Clock.Arm
  , dimensions      : Window.Dimensions
  , supportsP3Color : Bool
  , delta           : Float
  , state           : State
  }


type State
  = Paused
  | Playing



-- Flags


type alias Flags =
  { currentTime     : Int
  , dimensions      : Window.Dimensions
  , supportsP3Color : Bool
  }


flagsDecoder : Decoder Flags
flagsDecoder =
  Decode.map3 Flags
    ( Decode.field "currentTime" Decode.int )
    ( Decode.field "dimensions" <|
        Decode.map2 Window.Dimensions
          ( Decode.field "width"  Decode.int )
          ( Decode.field "height" Decode.int )
    )
    ( Decode.field "supportsP3Color" Decode.bool )



init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    time = Time.millisToPosix flags.currentTime

    zone = Time.utc

    datetime = Calendar.toDatetime zone time

    clockArms = Clock.init datetime
  in
  ( { datetime        = datetime
    , clockArms       = clockArms
    , dimensions      = flags.dimensions
    , supportsP3Color = flags.supportsP3Color
    , delta           = 0
    , state           = Playing
    }
  , Task.perform UpdateTimeZone Time.here
  )



-- Update


type Msg
  = UpdateTime Time.Posix
  | UpdateTimeZone Time.Zone
  | Animate Float
  | Resize Window.Dimensions
  | ToggleState Event.Visibility


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

    ToggleState visibility ->
      let
        state =
          case visibility of
            Event.Visible ->
              Playing

            Event.Hidden ->
              Paused

      in
      ( { model | state = state }
      , Task.perform UpdateTime Time.now
      )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Paused ->
      Sub.batch
        [ Event.onVisibilityChange ToggleState
        , Window.onResize Resize
        ]

    Playing ->
      Sub.batch
        [ Time.every 1000 UpdateTime
        , Event.onAnimationFrameDelta Animate
        , Event.onVisibilityChange ToggleState
        , Window.onResize Resize
        ]



-- View


view : Model -> Html Msg
view model =
  let
    { dimensions, clockArms, delta, supportsP3Color } = model
  in
  Html.div [] [ Draw.clock dimensions supportsP3Color clockArms delta ]
