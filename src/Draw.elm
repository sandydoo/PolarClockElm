module Draw exposing (..)


import Animation as Anim
import Svg exposing ( Svg )
import Svg.Attributes as SA

import Color

import Clock



-- Constants

tau = 2 * pi



type alias Arc =
  { startAngle   : Float
  , endAngle     : Float
  , radius       : Float
  , cornerRadius : Float
  , thickness    : Float
  }


--"#4a5568"
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



--colorFill : Float -> Lch
--colorFill =
--  Lch.interpolateLong
--    { l = 92.991667, c = 47.855050,  h = -30 }
--    { l = 92.991667, c = 47.855050,  h = 330 }


drawArm : Clock.Arm -> Float -> Svg msg
drawArm { radius, armRadius, animatedAngle, ticks } delta =
  let
    newAngle = Anim.animate delta animatedAngle

    progress =
      newAngle / 360

    ( cx, cy ) =
      pointOnArc 0 0 radius newAngle
  in
  Svg.path
    [ SA.d <|
      arcPath
        { startAngle = -armRadius / radius
        , endAngle = newAngle + headingChange armRadius radius
        , radius = radius
        , thickness = armRadius
        , cornerRadius = 23
        }
        --++ dotPath radius ( newAngle - ( armRadius / radius ) ) ( 0.8 * armRadius )
        ++ "Z"
    , SA.fill "#d9d9d9"
    , SA.fillRule "evenodd"
    ] []



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
arcPath arc =
  let
    cx = 0
    cy = 0

    { radius, thickness } = arc

    angleSpan =
      abs ( arc.endAngle - arc.startAngle )

    -- Diving by 2 later
    outerRadius = radius + thickness
    innerRadius = radius - thickness

    shouldCompleteCircle = angleSpan > ( 360 - 1.0e-6 )

    -- Remove division later
    cornerRadius =
      if shouldCompleteCircle then 0 else min ( max arc.cornerRadius 0 ) ( thickness / 2 )

    outerCornerOffsetAngle = headingChange cornerRadius outerRadius
    innerCornerOffsetAngle = headingChange cornerRadius innerRadius

    startAngle = arc.startAngle
    endAngle   = max arc.endAngle (outerCornerOffsetAngle * 2)

    ( outerStartX, outerStartY ) =
      pointOnArc cx cy outerRadius ( startAngle + outerCornerOffsetAngle )

    ( outerEndX, outerEndY ) =
      pointOnArc cx cy outerRadius ( endAngle - outerCornerOffsetAngle  )

    ( innerStartX, innerStartY ) =
      pointOnArc cx cy innerRadius ( startAngle + innerCornerOffsetAngle )

    ( innerEndX, innerEndY ) =
      pointOnArc cx cy innerRadius ( endAngle - innerCornerOffsetAngle )

    ( innerArcStartX, innerArcStartY ) =
      pointOnArc cx cy (innerRadius + cornerRadius) startAngle

    ( outerArcStartX, outerArcStartY ) =
      pointOnArc cx cy (outerRadius - cornerRadius) startAngle

    ( innerArcEndX, innerArcEndY ) =
      pointOnArc cx cy (innerRadius + cornerRadius) endAngle

    ( outerArcEndX, outerArcEndY ) =
      pointOnArc cx cy (outerRadius - cornerRadius) endAngle

    innerLargeArc =
      if angleSpan - 2 * innerCornerOffsetAngle > 180 then 1 else 0
    outerLargeArc =
      if angleSpan - 2 * outerCornerOffsetAngle > 180 then 1 else 0
  in
  if shouldCompleteCircle then
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
      , String.fromFloat cornerRadius
      , String.fromFloat cornerRadius
      , "0"
      , "0"
      , "1"
      , String.fromFloat outerStartX
      , String.fromFloat outerStartY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , String.fromFloat outerLargeArc
      , "1"
      , String.fromFloat outerEndX
      , String.fromFloat outerEndY
      , "A"
      , String.fromFloat cornerRadius
      , String.fromFloat cornerRadius
      , "0"
      , "0"
      , "1"
      , String.fromFloat outerArcEndX
      , String.fromFloat outerArcEndY
      , "L"
      , String.fromFloat innerArcEndX
      , String.fromFloat innerArcEndY
      , "A"
      , String.fromFloat cornerRadius
      , String.fromFloat cornerRadius
      , "0"
      , "0"
      , "1"
      , String.fromFloat innerEndX
      , String.fromFloat innerEndY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , String.fromFloat innerLargeArc
      , "0"
      , String.fromFloat innerStartX
      , String.fromFloat innerStartY
      , "A"
      , String.fromFloat cornerRadius
      , String.fromFloat cornerRadius
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


headingChange distance radius =
  ( distance / radius ) * ( 360 / tau )
