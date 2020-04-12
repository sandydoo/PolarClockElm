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
    difference = b - a
  in
  if difference == 0 then
    constant a

  else
    linear a difference


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
