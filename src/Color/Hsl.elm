module Color.HSL exposing (..)


import Color.Interpolate as Interpolate



type alias Hue =
  Float


type alias Saturation =
  Float


type alias Luminosity =
  Float


type HSL =
  HSL Hue Saturation Luminosity



-- Interpolate


interpolate : HSL -> HSL -> Float -> HSL
interpolate (HSL hue1 sat1 lum1) (HSL hue2 sat2 lum2) time =
  HSL
    ( Interpolate.hue   hue1 hue2 time )
    ( Interpolate.float sat1 sat2 time )
    ( Interpolate.float lum1 lum2 time )



-- Serialize


toString (HSL hue sat lum) =
  "hsl(" ++ String.fromFloat hue ++ ", " ++ String.fromFloat sat ++ "%, " ++ String.fromFloat lum ++ "%)"
