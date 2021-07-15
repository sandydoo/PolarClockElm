module Clock exposing (..)

import Svg exposing ( Svg )
import Svg.Attributes as SA

import Calendar exposing ( Unit(..) )
import Color
import Draw



type alias Arm =
  { radius      : Float
  , thickness   : Float
  , angle       : Float
  , ticks       : Ticks
  , fill        : Color.Color
  , stroke      : Color.Color
  , strokeWidth : Float
  }


type alias Ticks =
  { unit     : Unit
  , count    : Int
  , labels   : List String
  , subUnit  : Unit
  , subCount : Int
  }


createTicks : Unit -> Calendar.DateTime -> Ticks
createTicks unit datetime =
  let
    labels = Calendar.range unit datetime
    subUnit = Calendar.subUnitOf unit
  in
  { unit     = unit
  , count    = List.length labels
  , labels   = labels
  , subUnit  = subUnit
  , subCount = List.length <| Calendar.range subUnit datetime
  }


updateTicks : Ticks -> Calendar.DateTime -> Ticks
updateTicks ticks datetime =
  let
    { unit } = ticks
  in
  case unit of
    Days ->
      createTicks Days datetime

    _ ->
      ticks



init : Calendar.DateTime -> List Arm
init datetime =
  let
    thickness = 23
  in
  [ { radius = 100
    , thickness = thickness
    , ticks = createTicks Months datetime
    , angle = 0
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  , { radius = 155
    , thickness = thickness
    , ticks = createTicks Weekdays datetime
    , angle = 0
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  , { radius = 210
    , thickness = thickness
    , ticks = createTicks Days datetime
    , angle = 0
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  , { radius = 300
    , thickness = thickness
    , ticks = createTicks Hours datetime
    , angle = 0
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  , { radius = 355
    , thickness = thickness
    , ticks = createTicks Minutes datetime
    , angle = 0
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  , { radius = 410
    , thickness = thickness
    , ticks = createTicks Seconds datetime
    , angle = 0
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  ]



update : Calendar.DateTime -> List Arm -> List Arm
update datetime arms =
  let
    updateEach ({ ticks } as arm) =
      let
        newTicks = updateTicks ticks datetime

        newAngle = calculateAngle datetime newTicks
      in
      { arm | ticks = newTicks, angle = newAngle }

  in
  List.map updateEach arms


calculateAngle : Calendar.DateTime -> Ticks -> Float
calculateAngle datetime ticks =
  let
    newAngle = toFloat ( Calendar.toPart ticks.unit datetime ) / toFloat ticks.count * 360

    newSubAngle =
      (toFloat ( Calendar.toPart ticks.subUnit datetime ) / toFloat ticks.subCount ) * (360 / toFloat ticks.count)
  in
    newAngle + newSubAngle



-- View


drawArm : Arm -> Svg msg
drawArm { radius, thickness, angle, fill, stroke, strokeWidth } =
  Svg.path
    [ SA.d <|
      Draw.arc
        { startAngle = 0---thickness / radius
        , endAngle = angle --+ headingChange thickness radius
        , radius = radius
        , thickness = thickness
        , cornerRadius = 8
        }
        --++ Draw.dot radius ( angle - ( thickness / radius ) ) ( 0.8 * thickness )
        ++ "Z"
    , SA.fill <| Color.toCssString fill
    , SA.fillRule "evenodd"
    , SA.stroke <| Color.toCssString stroke
    , SA.strokeWidth <| String.fromFloat strokeWidth
    ] []


drawCursor : Arm -> Svg msg
drawCursor { radius, thickness, angle } =
  let
    ( cx, cy ) = Draw.pointOnArc 0 0 radius angle

    translate =
      "translate(" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy ++ ")"
  in
  Svg.g
    [ SA.transform translate ]
    [ Svg.circle
      [ SA.class "clock-track-hide-overflow"
      , SA.cx "0"
      , SA.cy "0"
      , SA.r <| String.fromFloat thickness
      ] []
    ]


drawTrack : Float -> Color.Color -> Float -> Svg msg
drawTrack radius strokeColor strokeWidth =
  Svg.circle
    [ SA.cx "0"
    , SA.cy "0"
    , SA.r ( String.fromFloat radius )
    , SA.fill "none"
    , SA.stroke ( Color.toCssString strokeColor )
    , SA.strokeWidth ( String.fromFloat strokeWidth )
    , SA.class "clock-track"
    ] []


drawTicks : Arm -> List ( Svg msg )
drawTicks { radius, thickness, ticks } =
  let
    drawTick index tick =
      let
        ( cx, cy ) =
          Draw.pointOnArc 0 0 radius ( toFloat index / toFloat ticks.count * 360 )

        translateTick =
          "translate(" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy ++ ")"

        fontSize = String.fromFloat ( 0.6 * thickness ) ++ "px"
      in
      Svg.g
        [ SA.transform translateTick
        , SA.class "clock-tick"
        ]
        --[ Svg.circle
        --  [ SA.cx "0"
        --  , SA.cy "0"
        --  , SA.r <| String.fromFloat ( 0.7 * thickness )
        --  ] []
        [ Svg.text_
          [ SA.dy "0.35em"
          , SA.fontSize fontSize
          ]
          [ Svg.text tick ]
        ]

  in
  List.indexedMap drawTick ticks.labels



-- DRAFT: Themes

order : List Unit
order =
  [ Months
  , Weekdays
  , Days
  , Hours
  , Minutes
  , Seconds
  ]

original =
  [
    { radius = 200
    , thickness = 23
    , cornerRadius = 23
    , fill = Color.blue
    , stroke = Color.none
    , strokeWidth = 0
    }
  ]
