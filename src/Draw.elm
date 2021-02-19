module Draw exposing (..)


import Animation as Anim
import Svg
import Svg.Attributes as SA
import Svg.Lazy as SL

import Color.Rgb as Rgb
import Color.Lab exposing (Lab)
import Color.Lab as Lab
import Color.Interpolate as Interpolate



-- Constants

tau = 2 * pi



type alias Arc =
  { startAngle : Float
  , endAngle : Float
  , innerRadius : Float
  , outerRadius : Float
  , cornerRadius : Float
  }


drawClock { clockArms, dimensions, delta } =
  Svg.svg
    [ SA.width  <| String.fromInt dimensions.width
    , SA.height <| String.fromInt dimensions.height
    , SA.viewBox "0 0 1000 1000"
    , SA.preserveAspectRatio "xMidYMid meet"
    ]
    [ Svg.g
      [ SA.transform "translate(500, 500)" ]
      [ SL.lazy  drawTracks clockArms
      , SL.lazy2 drawTrackHidingCircles delta clockArms
      , SL.lazy  drawTicks clockArms
      , SL.lazy2 drawArms delta clockArms
      ]
    ]


drawTrack radius =
  Svg.circle
    [ SA.cx "0"
    , SA.cy "0"
    , SA.r <|
        String.fromFloat radius
    , SA.fill "none"
    , SA.stroke "#4a5568"
    , SA.strokeWidth "1"
    , SA.class "clock-track"
    ] []


drawTracks clockArms =
  Svg.g [] <|
    List.map (.radius >> drawTrack) clockArms


drawTrackHidingCircles delta clockArms =
  let
    drawCircle { angle, armRadius, radius } =
      let
        ( cx, cy ) = pointOnArc 0 0 radius (Anim.animate delta angle)

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

  in
  Svg.g [] <|
    List.map drawCircle clockArms


drawTicks_ { range, length, radius, armRadius, angle } =
  let
    drawText { cx, cy, tick } =
      let
        translateTick =
          "translate(" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy ++ ")"

        fontSize = String.fromFloat (0.6 * armRadius) ++ "px"
      in
      Svg.g
        [ SA.transform translateTick
        , SA.class "clock-tick"
        ]
        [ Svg.circle
          [ SA.cx "0"
          , SA.cy "0"
          , SA.r <| String.fromFloat (0.7 * armRadius)
          ] []
        , Svg.text_
          [ SA.dy "0.35em"
          , SA.fontSize fontSize
          ]
          [ Svg.text tick ]
        ]

    positionTicksOnArc index tick =
      let
        ( cx, cy ) =
          pointOnArc 0 0 radius (toFloat index / toFloat length * 360)
      in
      { tick = tick, cx = cx, cy = cy }

    newRange =
      List.indexedMap positionTicksOnArc range
  in
  List.map drawText newRange


drawTicks clockArms =
  Svg.g [] <|
    List.concatMap drawTicks_ clockArms


drawDot radius angle dotRadius =
  let
    ( cx, cy ) =
      pointOnArc 0 0 radius angle

    strDotRadius =
      String.fromFloat dotRadius
  in
  String.join " "
    [ "M"
    , String.fromFloat cx
    , String.fromFloat (dotRadius + cy)
    , "a"
    , strDotRadius
    , strDotRadius
    , "0"
    , "0"
    , "1"
    , "0"
    , String.fromFloat (-dotRadius * 2)
    , "a"
    , strDotRadius
    , strDotRadius
    , "0"
    , "0"
    , "1"
    , "0"
    , String.fromFloat (dotRadius * 2)
    ]


colorFill : Float -> Lab
colorFill =
  Lab.interpolate
    ( Lab.fromRgb { r = 252, g = 222, b = 156 } )
    ( Lab.fromRgb { r = 251, g = 183, b = 192 } )


drawArm delta { radius, armRadius, angle } =
  let
    newAngle = Anim.animate delta angle

    progress =
      newAngle / 360

    fill =
      ( Lab.toRgb >> Rgb.toString ) <| colorFill progress

    ( cx, cy ) =
      pointOnArc 0 0 radius newAngle
  in
  Svg.g []
    [ Svg.path
      [ SA.d <|
        drawArc
          { startAngle = -armRadius / radius
          , endAngle = newAngle + (armRadius / radius)
          , innerRadius = radius - armRadius
          , outerRadius = radius + armRadius
          , cornerRadius = armRadius
          }
          ++ drawDot radius (newAngle - (armRadius / radius)) (0.8 * armRadius)
          ++ "Z"
      , SA.fill fill
      , SA.fillRule "evenodd"
      ] []
    ]


drawArms delta clockArms =
  Svg.g [] <|
    List.map (drawArm delta) clockArms


pointOnArc : Float -> Float -> Float -> Float -> (Float, Float)
pointOnArc cx cy radius angle =
  let
    radAngle = degrees angle - pi / 2
  in
    ( cx + radius * cos radAngle
    , cy + radius * sin radAngle
    )


drawArc : Arc -> String
drawArc { startAngle, endAngle, innerRadius, outerRadius, cornerRadius } =
  let
    cx = 0
    cy = 0

    deltaAngle =
      abs (endAngle - startAngle)

    radius =
      (outerRadius + innerRadius) / 2

    width =
      outerRadius - innerRadius

    corner =
      min (width / 2) cornerRadius

    ( oStartX, oStartY ) =
      pointOnArc cx cy outerRadius startAngle

    ( oEndX, oEndY ) =
      pointOnArc cx cy outerRadius endAngle

    ( iStartX, iStartY ) =
      pointOnArc cx cy innerRadius startAngle

    ( iEndX, iEndY ) =
      pointOnArc cx cy innerRadius endAngle

    cornerDeltaAngle =
      360 * (corner / (tau * radius))

    ( iArcStartX, iArcStartY ) =
      pointOnArc cx cy radius (startAngle - cornerDeltaAngle)

    ( iArcEndX, iArcEndY ) =
      pointOnArc cx cy radius (endAngle + cornerDeltaAngle)

    ( oArcStartX, oArcStartY ) =
      pointOnArc cx cy radius (startAngle - cornerDeltaAngle)

    ( oArcEndX, oArcEndY ) =
      pointOnArc cx cy radius (endAngle + cornerDeltaAngle)

    arcSweep =
      if deltaAngle > 180 then
        1

      else
        0
  in
  if (deltaAngle + 2 * cornerDeltaAngle) > (360 - 1.0e-6) then
    let
      ( oHalfX, oHalfY ) =
        pointOnArc cx cy outerRadius (startAngle + 180)

      ( iHalfX, iHalfY ) =
        pointOnArc cx cy innerRadius (startAngle + 180)
    in
    String.join " "
      [ "M"
      , String.fromFloat oStartX
      , String.fromFloat oStartY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat oHalfX
      , String.fromFloat oHalfY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat oStartX
      , String.fromFloat oStartY
      , "M"
      , String.fromFloat iStartX
      , String.fromFloat iStartY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat iHalfX
      , String.fromFloat iHalfY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , "1"
      , "0"
      , String.fromFloat iStartX
      , String.fromFloat iStartY
      ]

  else
    String.join " "
      [ "M"
      , String.fromFloat oArcStartX
      , String.fromFloat oArcStartY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat oStartX
      , String.fromFloat oStartY
      , "A"
      , String.fromFloat outerRadius
      , String.fromFloat outerRadius
      , "0"
      , String.fromFloat arcSweep
      , "1"
      , String.fromFloat oEndX
      , String.fromFloat oEndY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat oArcEndX
      , String.fromFloat oArcEndY
      , "L"
      , String.fromFloat iArcEndX
      , String.fromFloat iArcEndY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat iEndX
      , String.fromFloat iEndY
      , "A"
      , String.fromFloat innerRadius
      , String.fromFloat innerRadius
      , "0"
      , String.fromFloat arcSweep
      , "0"
      , String.fromFloat iStartX
      , String.fromFloat iStartY
      , "A"
      , String.fromFloat corner
      , String.fromFloat corner
      , "0"
      , "0"
      , "1"
      , String.fromFloat iArcStartX
      , String.fromFloat iArcStartY
      ]
