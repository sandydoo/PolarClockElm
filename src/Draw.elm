module Draw exposing (..)


import Animation as Anim
import Svg exposing ( Svg )
import Svg.Attributes as SA

import Color.Rgb as Rgb
import Color.Lab as Lab
import Color.Lch as Lch exposing ( Lch )

import Clock



-- Constants

tau = 2 * pi



type alias Arc =
  { startAngle   : Float
  , endAngle     : Float
  , innerRadius  : Float
  , outerRadius  : Float
  , cornerRadius : Float
  }



drawTrack : Float -> Svg msg
drawTrack radius =
  Svg.circle
    [ SA.cx "0"
    , SA.cy "0"
    , SA.r <| String.fromFloat radius
    , SA.fill "none"
    , SA.stroke "#4a5568"
    , SA.strokeWidth "1"
    , SA.class "clock-track"
    ] []



drawCursor : Clock.Arm -> Float -> Svg msg
drawCursor { radius, armRadius, animatedAngle } delta =
  let
    ( cx, cy ) = pointOnArc 0 0 radius ( Anim.animate delta animatedAngle )

    translate =
      "translate(" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy ++ ")"
  in
  Svg.g
    [ SA.transform translate ]
    [ Svg.circle
      [ SA.class "clock-track-hide-overflow"
      , SA.cx "0"
      , SA.cy "0"
      , SA.r <| String.fromFloat armRadius
      ] []
    ]


drawTicks : Clock.Arm -> List ( Svg msg )
drawTicks { radius, armRadius, ticks } =
  let
    drawTick index tick =
      let
        ( cx, cy ) =
          pointOnArc 0 0 radius ( toFloat index / toFloat ticks.count * 360 )

        translateTick =
          "translate(" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy ++ ")"

        fontSize = String.fromFloat ( 0.6 * armRadius ) ++ "px"
      in
      Svg.g
        [ SA.transform translateTick
        , SA.class "clock-tick"
        ]
        [ Svg.circle
          [ SA.cx "0"
          , SA.cy "0"
          , SA.r <| String.fromFloat ( 0.7 * armRadius )
          ] []
        , Svg.text_
          [ SA.dy "0.35em"
          , SA.fontSize fontSize
          ]
          [ Svg.text tick ]
        ]

  in
  List.indexedMap drawTick ticks.labels



colorFill : Float -> Lch
colorFill =
  Lch.interpolateLong
    { l = 92.991667, c = 47.855050,  h = -30 }
    { l = 92.991667, c = 47.855050,  h = 330 }


drawArm : Clock.Arm -> Bool -> Float -> Svg msg
drawArm { radius, armRadius, animatedAngle } supportsP3Color delta =
  let
    newAngle = Anim.animate delta animatedAngle

    progress =
      newAngle / 360

    fill =
      ( Lch.toLab >> Lab.toRgb ) <| colorFill progress

    ( cx, cy ) =
      pointOnArc 0 0 radius newAngle
  in
  Svg.g []
    [ Svg.path
      [ SA.d <|
        arcPath
          { startAngle = -armRadius / radius
          , endAngle = newAngle + ( armRadius / radius )
          , innerRadius = radius - armRadius
          , outerRadius = radius + armRadius
          , cornerRadius = armRadius
          }
          ++ dotPath radius ( newAngle - ( armRadius / radius ) ) ( 0.8 * armRadius )
          ++ "Z"
      , SA.fill <|
          if supportsP3Color
          then Rgb.toP3String fill
          else Rgb.toString fill
      , SA.fillRule "evenodd"
      ] []
    ]



-- SVG Paths


dotPath : Float -> Float -> Float -> String
dotPath radius angle dotRadius =
  let
    ( cx, cy ) =
      pointOnArc 0 0 radius angle

    dotRadiusString =
      String.fromFloat dotRadius
  in
  String.join " "
    [ "M"
    , String.fromFloat cx
    , String.fromFloat <| dotRadius + cy
    , "a"
    , dotRadiusString
    , dotRadiusString
    , "0"
    , "0"
    , "1"
    , "0"
    , String.fromFloat <| -dotRadius * 2
    , "a"
    , dotRadiusString
    , dotRadiusString
    , "0"
    , "0"
    , "1"
    , "0"
    , String.fromFloat <| dotRadius * 2
    ]


arcPath : Arc -> String
arcPath { startAngle, endAngle, innerRadius, outerRadius, cornerRadius } =
  let
    cx = 0
    cy = 0

    deltaAngle =
      abs ( endAngle - startAngle )

    radius =
      ( outerRadius + innerRadius ) / 2

    width =
      outerRadius - innerRadius

    corner =
      min ( width / 2 ) cornerRadius

    ( outerStartX, outerStartY ) =
      pointOnArc cx cy outerRadius startAngle

    ( outerEndX, outerEndY ) =
      pointOnArc cx cy outerRadius endAngle

    ( innerStartX, innerStartY ) =
      pointOnArc cx cy innerRadius startAngle

    ( innerEndX, innerEndY ) =
      pointOnArc cx cy innerRadius endAngle

    cornerDeltaAngle =
      360 * ( corner / ( tau * radius ) )

    ( innerArcStartX, innerArcStartY ) =
      pointOnArc cx cy radius ( startAngle - cornerDeltaAngle )

    ( innerArcEndX, innerArcEndY ) =
      pointOnArc cx cy radius ( endAngle + cornerDeltaAngle )

    ( outerArcStartX, outerArcStartY ) =
      pointOnArc cx cy radius ( startAngle - cornerDeltaAngle )

    ( outerArcEndX, outerArcEndY ) =
      pointOnArc cx cy radius ( endAngle + cornerDeltaAngle )

    arcSweep =
      if deltaAngle > 180 then
        1

      else
        0
  in
  if ( deltaAngle + 2 * cornerDeltaAngle ) > ( 360 - 1.0e-6 ) then
    let
      ( outerHalfX, outerHalfY ) =
        pointOnArc cx cy outerRadius ( startAngle + 180 )

      ( innerHalfX, innerHalfY ) =
        pointOnArc cx cy innerRadius ( startAngle + 180 )
    in
    String.join " "
      [ "M"
      , String.fromFloat outerStartX
      , String.fromFloat outerStartY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat outerHalfX
      , String.fromFloat outerHalfY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat outerStartX
      , String.fromFloat outerStartY
      , "M"
      , String.fromFloat innerStartX
      , String.fromFloat innerStartY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat innerHalfX
      , String.fromFloat innerHalfY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat innerStartX
      , String.fromFloat innerStartY
      ]

  else
    String.join " "
      [ "M"
      , String.fromFloat outerArcStartX
      , String.fromFloat outerArcStartY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat outerStartX
      , String.fromFloat outerStartY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , String.fromFloat arcSweep
      , "1"
      , String.fromFloat outerEndX
      , String.fromFloat outerEndY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat outerArcEndX
      , String.fromFloat outerArcEndY
      , "L"
      , String.fromFloat innerArcEndX
      , String.fromFloat innerArcEndY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat innerEndX
      , String.fromFloat innerEndY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , String.fromFloat arcSweep
      , "0"
      , String.fromFloat innerStartX
      , String.fromFloat innerStartY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat innerArcStartX
      , String.fromFloat innerArcStartY
      ]



-- Utilities


pointOnArc : Float -> Float -> Float -> Float -> ( Float, Float )
pointOnArc cx cy radius angle =
  let
    radAngle = degrees angle - pi / 2
  in
    ( cx + radius * cos radAngle
    , cy + radius * sin radAngle
    )
