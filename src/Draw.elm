module Draw exposing (..)


import Animation as Anim
import Color
import Svg
import Svg.Attributes as SA
import Svg.Lazy as SL



-- Constants

tau = 2 * pi
epsilon = 1.0e-6
tauEpsilon = tau - epsilon


type alias Arc =
  { startAngle : Float
  , endAngle : Float
  , innerRadius : Float
  , outerRadius : Float
  , cornerRadius : Float
  }


drawClock {clockArms, width, height, delta} =
  let
    viewbox =
      "0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height

    translate =
      "translate(" ++ String.fromInt (width // 2) ++ "," ++ String.fromInt (height // 2) ++ ")"
  in
  Svg.svg
    [ SA.width (String.fromInt width)
    , SA.height (String.fromInt height)
    , SA.viewBox viewbox
    ]
    [ Svg.g
      [ SA.transform <| translate
      ]
      [ SL.lazy drawTracks clockArms
      , SL.lazy2 drawTrackHidingCircles delta clockArms
      , SL.lazy drawTicks_ clockArms
      , SL.lazy2 drawArms delta clockArms
      ]
    ]


drawTrack radius =
  Svg.circle
    [ SA.cx "0"
    , SA.cy "0"
    , SA.r (String.fromFloat radius)
    , SA.class "clock-track"
    ]
    []


drawTracks clockArms =
  Svg.g [] (List.map (.radius >> drawTrack) clockArms)


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
          ]
          []
        ]

  in
  Svg.g [] <| List.map drawCircle clockArms


drawTicks { interval, length, radius, armRadius, angle } =
  let
    drawText { cx, cy, tick } =
      let
        translateTick =
          "translate(" ++ String.fromFloat cx ++ "," ++ String.fromFloat cy ++ ")"

        fontSize = String.fromFloat (0.6 * armRadius) ++ "px"
      in
      Svg.g
        [ SA.transform <| translateTick
        , SA.class "clock-tick"
        ]
        [ Svg.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| String.fromFloat (0.7 * armRadius)
            ]
            []
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
      List.indexedMap positionTicksOnArc interval
  in
  List.map drawText newRange


drawTicks_ clockArms =
  Svg.g [] (List.concatMap drawTicks clockArms)


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


drawArm delta { radius, armRadius, angle } =
  let
    progress =
      Anim.animate delta angle / 360

    fill =
      Color.stringFromHSL <|
        Color.interpolateHSL (Color.HSL 0 76.4 75.1) (Color.HSL 360 76.4 75.1) progress

    ( cx, cy ) = pointOnArc 0 0 radius (Anim.animate delta angle)
  in
  Svg.g []
    [ Svg.path
      [ SA.d <|
        drawArc
          { startAngle = -armRadius / radius
          , endAngle = Anim.animate delta angle + (armRadius / radius)
          , innerRadius = radius - armRadius
          , outerRadius = radius + armRadius
          , cornerRadius = armRadius
          }
          ++ drawDot radius (Anim.animate delta angle - (armRadius / radius)) (0.8 * armRadius)
          ++ "Z"
      , SA.fill fill
      , SA.fillRule "evenodd"
      ]
      []
    ]


drawArms delta clockArms =
  Svg.g [] <| List.map (drawArm delta) clockArms


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
