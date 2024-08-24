module Color.Lch exposing (..)

import Color.Interpolate as Interpolate
import Color.Lab exposing (Lab)


type alias Lch =
    { l : Float, c : Float, h : Float }


type alias Hcl =
    Lch


fromLab : Lab -> Lch
fromLab { l, a, b } =
    let
        c =
            sqrt (a ^ 2 + b ^ 2)

        theta =
            atan2 b a * 180 / pi

        h =
            if theta > 0 then
                theta

            else
                theta + 360
    in
    { l = l
    , c = c
    , h = h
    }


toLab : Lch -> Lab
toLab { l, c, h } =
    let
        hRad =
            h * pi / 180
    in
    { l = l
    , a = c * cos hRad
    , b = c * sin hRad
    }



-- Interpolate


interpolate : Lch -> Lch -> Float -> Lch
interpolate lch1 lch2 time =
    { l = Interpolate.float lch1.l lch2.l time
    , c = Interpolate.float lch1.c lch2.c time
    , h = Interpolate.hue lch1.h lch2.h time
    }


interpolateLong : Lch -> Lch -> Float -> Lch
interpolateLong lch1 lch2 time =
    { l = Interpolate.float lch1.l lch2.l time
    , c = Interpolate.float lch1.c lch2.c time
    , h = Interpolate.float lch1.h lch2.h time
    }
