module Clock exposing (..)

import Animation as Anim
import Ease exposing ( outQuart )
import Time

import Calendar
import Calendar exposing ( DateTime, Unit(..) )



type alias Arm =
  { radius        : Float
  , armRadius     : Float
  , ticks         : Ticks
  , animatedAngle : Anim.Animation
  }


type alias Ticks =
  { unit   : Unit
  , count  : Int
  , labels : List String
  }


createTicks : Unit -> DateTime -> Ticks
createTicks unit datetime =
  let
    labels = Calendar.range unit datetime
  in
  { unit   = unit
  , count  = List.length labels
  , labels = labels
  }


updateTicks : Ticks -> DateTime -> Ticks
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
    armRadius = 23
  in
  [ { radius = 100
    , armRadius = armRadius
    , ticks = createTicks Months datetime
    , animatedAngle = Anim.static 0
    }
  , { radius = 155
    , armRadius = armRadius
    , ticks = createTicks Weekdays datetime
    , animatedAngle = Anim.static 0
    }
  , { radius = 210
    , armRadius = armRadius
    , ticks = createTicks Days datetime
    , animatedAngle = Anim.static 0
    }
  , { radius = 300
    , armRadius = armRadius
    , ticks = createTicks Hours datetime
    , animatedAngle = Anim.static 0
    }
  , { radius = 355
    , armRadius = armRadius
    , ticks = createTicks Minutes datetime
    , animatedAngle = Anim.static 0
    }
  , { radius = 410
    , armRadius = armRadius
    , ticks = createTicks Seconds datetime
    , animatedAngle = Anim.static 0
    }
  ]



update : Calendar.DateTime -> Float -> List Arm -> List Arm
update datetime delta arms =
  let
    updateEach arm =
      let
        { ticks, animatedAngle } = arm

        newTicks = updateTicks ticks datetime

        newAngle =
          toFloat ( Calendar.toPart ticks.unit datetime ) / toFloat newTicks.count * 360

        newAnimatedAngle =
          Anim.retarget delta newAngle animatedAngle
          |> Anim.duration 750
          |> Anim.ease outQuart

      in
      { arm | ticks = newTicks, animatedAngle = newAnimatedAngle }

  in
  List.map updateEach arms
