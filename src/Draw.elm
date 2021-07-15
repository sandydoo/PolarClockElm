module Draw exposing (..)



-- Constants

tau : Float
tau = 2 * pi



type alias Arc =
  { startAngle   : Float
  , endAngle     : Float
  , radius       : Float
  , cornerRadius : Float
  , thickness    : Float
  }



-- SVG Paths


dot : Float -> Float -> Float -> String
dot radius angle dotRadius =
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


arc : Arc -> String
arc arcSpec =
  let
    cx = 0
    cy = 0

    { radius, thickness } = arcSpec

    angleSpan =
      abs ( arcSpec.endAngle - arcSpec.startAngle )

    outerRadius = radius + thickness
    innerRadius = radius - thickness

    shouldCompleteCircle = angleSpan > ( 360 - 1.0e-6 )

    cornerRadius =
      if shouldCompleteCircle then 0 else min ( max arcSpec.cornerRadius 0 ) thickness

    outerCornerOffsetAngle = headingChange cornerRadius outerRadius
    innerCornerOffsetAngle = headingChange cornerRadius innerRadius

    startAngle = arcSpec.startAngle
    endAngle   = max arcSpec.endAngle (outerCornerOffsetAngle * 2)

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



-- Calculations


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
