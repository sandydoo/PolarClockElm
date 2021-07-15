module Main exposing (..)


import Browser
import Browser.Events as Event
import Json.Decode as Decode exposing ( Decoder )
import Html exposing ( Html )
import Svg
import Svg.Attributes as SA
import Task
import Time

import Calendar exposing ( DateTime(..) )
import Clock
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

type alias Model =
  { datetime  : Calendar.DateTime
  , clock     : Clock
  , config    : Config
  , status    : Status
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
    datetime = Calendar.toDatetime Time.utc <| Time.millisToPosix flags.currentTime

    config =
      { dimensions = flags.dimensions
      , colorSpace = if flags.supportsP3Color then DisplayP3 else SRGB
      }
  in
  ( { datetime  = datetime
    , clock     = Clock.init datetime
    , config    = config
    , status    = Playing
    }
  , Task.perform UpdateTimeZone Time.here
  )



-- Update


type Msg
  = UpdateTime Time.Posix
  | UpdateTimeZone Time.Zone
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

        newClock = Clock.update newDatetime model.clock
      in
      ( { model | datetime = newDatetime, clock = newClock }
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
        newStatus =
          case visibility of
            Event.Visible ->
              Playing

            Event.Hidden ->
              Paused

      in
      ( { model | status = newStatus }
      , Task.perform UpdateTime Time.now
      )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions { status } =
  case status of
    Paused ->
      Sub.batch
        [ Event.onVisibilityChange ToggleState
        , Window.onResize Resize
        ]

    Playing ->
      Sub.batch
        [ Event.onAnimationFrame UpdateTime
        , Event.onVisibilityChange ToggleState
        , Window.onResize Resize
        ]



-- View


view : Model -> Html Msg
view model =
  let
    { width, height } = model.config.dimensions

    thickness = 23
    radius = 200
    angle = 180
  in
  Html.div []
    [ Svg.svg
      [ SA.width  ( String.fromInt width  )
      , SA.height ( String.fromInt height )
      , SA.viewBox "0 0 1000 1000"
      , SA.preserveAspectRatio "xMidYMid meet"
      ]
      [ Svg.g
        [ SA.transform "translate(500, 500)" ] <|
        --[ Svg.path
        --  [ SA.d <|
        --    Draw.arcPath
        --      { startAngle = 0
        --      , endAngle = angle
        --      , radius = radius
        --      , thickness = thickness
        --      , cornerRadius = 0
        --      }
        --      --++ dotPath radius ( newAngle - ( armRadius / radius ) ) ( 0.8 * armRadius )
        --      ++ "Z"
        --  , SA.fill "#d9d9d9"
        --  , SA.fillRule "evenodd"
        --  , SA.stroke "black"
        --  , SA.strokeWidth "2"
        --  ] []
        --]
        flip List.map model.clock <| \arm ->
          Svg.g []
          --[ Draw.drawTrack arm.radius (Color.rgb 1.0 1.0 1.0) 0.5
          --, Svg.g [] <| Draw.drawTicks arm
          --, Draw.drawArm arm animation.delta
          --]
          [ Clock.drawArm arm
          ]
      ]
    ]


flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b
