module Color.DisplayP3 exposing (..)

import Color.Rgb exposing ( RGB )



toCssString : RGB -> String
toCssString { r, g, b }
  = "color(display-p3 "
  ++ String.fromFloat r
  ++ " "
  ++ String.fromFloat g
  ++ " "
  ++ String.fromFloat b
  ++ ")"
