module Color.Hsl exposing (..)

import Color.Interpolate as Interpolate


type alias HSL =
    { h : Float, s : Float, l : Float }



-- Interpolate


interpolate : HSL -> HSL -> Float -> HSL
interpolate hsl1 hsl2 time =
    { h = Interpolate.hue hsl1.h hsl2.h time
    , s = Interpolate.float hsl1.s hsl2.s time
    , l = Interpolate.float hsl1.l hsl2.l time
    }



-- Serialize


toString : HSL -> String
toString { h, s, l } =
    "hsl("
        ++ String.fromFloat h
        ++ ", "
        ++ String.fromFloat s
        ++ "%, "
        ++ String.fromFloat l
        ++ "%)"
