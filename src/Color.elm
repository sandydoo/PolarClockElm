module Color exposing (HSL(..), interpolateHSL, stringFromHSL)



type alias Hue =
  Float


type alias Saturation =
  Float


type alias Luminosity =
  Float


type HSL =
  HSL Hue Saturation Luminosity



-- Interpolation


interpolateHSL (HSL hue1 sat1 lum1) (HSL hue2 sat2 lum2) time =
  HSL (interpolateHue hue1 hue2 time) (interpolateFloat sat1 sat2 time) (interpolateFloat lum1 lum2 time)


interpolateHue : Hue -> Hue -> Float -> Hue
interpolateHue a b =
  let
    d =
      if b > a && b - a > 180 then
        b - a + 360
      else if b < a && a - b > 180 then
        b + 360 - a
      else
        b - a
  in
  linear a d


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat start end time =
  linear start (end - start) time


constant : a -> t -> a
constant a _ = a


linear : Float -> Float -> Float -> Float
linear a d t =
  a + d * t



-- Serialization


stringFromHSL (HSL hue sat lum) =
  "hsl(" ++ String.fromFloat hue ++ ", " ++ String.fromFloat sat ++ "%, " ++ String.fromFloat lum ++ "%)"
