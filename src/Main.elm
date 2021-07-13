module Main exposing (..)


import Browser
import Browser.Events as Event
import Json.Decode as Decode exposing ( Decoder )
import Html exposing ( Html )
import Svg exposing ( Svg )
import Svg.Attributes as SA
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

type alias Clock = List Clock.Arm
type alias Controls = {}

type ColorSpace
  = SRGB
  | DisplayP3

type alias Config =
  { dimensions : Window.Dimensions
  , colorSpace : ColorSpace
  }

type Status
  = Paused
  | Playing

type alias Animation =
  { delta  : Float
  , status : Status
  }

type alias Model =
  { datetime  : Calendar.DateTime
  , clock     : Clock
  , config    : Config
  , animation : Animation
  }



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

    config =
      { dimensions = flags.dimensions
      , colorSpace = if flags.supportsP3Color then DisplayP3 else SRGB
      }

    animation =
      { delta  = 0
      , status = Playing
      }
  in
  ( { datetime  = datetime
    , clock     = Clock.init datetime
    , config    = config
    , animation = animation
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

        --newClockArms = Clock.update newDatetime model.delta model.clockArms
      in
      ( { model | datetime = newDatetime, clock = model.clock }
      , Cmd.none
      )

    Animate newDelta ->
      let
        animation = model.animation
        newAnimation = { animation | delta = animation.delta + newDelta }
      in
      ( { model | animation = newAnimation }
      , Cmd.none
      )

    Resize newDimensions ->
      let
        { config } = model

        newConfig = { config | dimensions = newDimensions }
      in
      ( { model | config = newConfig }
      , Cmd.none
      )

    ToggleState visibility ->
      let
        { animation } = model

        status =
          case visibility of
            Event.Visible ->
              Playing

            Event.Hidden ->
              Paused

      in
      ( { model | animation = { animation | status = status } }
      , Task.perform UpdateTime Time.now
      )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions { animation } =
  case animation.status of
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
    { width, height } = model.config.dimensions
  in
  Html.div []
    [ Svg.svg
      [ SA.width  ( String.fromInt width  )
      , SA.height ( String.fromInt height )
      , SA.viewBox "0 0 1000 1000"
      , SA.preserveAspectRatio "xMidYMid meet"
      ]
      [ Svg.g
        [ SA.transform "translate(500, 500)" ]
        ( List.map (.radius >> Draw.drawTrack) model.clock )
      ]
    ]
