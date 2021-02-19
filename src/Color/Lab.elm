module Color.Lab exposing (..)


import Color.Interpolate as Interpolate
import Color.Rgb exposing (RGB)
import Color.Xyz exposing (XYZ)
import Color.Xyz as Xyz


-- See http://www.brucelindbloom.com/index.html?Eqn_Lab_to_XYZ.html for an
-- explanation of these constants.
epsilon = 216 / 24389
kappa   = 24389 / 27

xRef = 95.047
yRef = 100
zRef = 108.883


type alias Lab =
  { l: Float, a: Float, b: Float }


fromRgb : RGB -> Lab
fromRgb = Xyz.fromRgb >> fromXyz


fromXyz : XYZ -> Lab
fromXyz { x, y, z } =
  let
    f a =
      if a > epsilon then
        a ^ (1 / 3)

      else
        ( kappa * a + 16 ) / 116

    fx = f ( x / xRef )

    fy = f ( y / yRef )

    fz = f ( z / zRef )
  in
    { l = 116 * fy - 16
    , a = 500 * ( fx - fy )
    , b = 200 * ( fy - fz )
    }



toXyz : Lab -> XYZ
toXyz { l, a, b } =
  let
    f v =
      let
        v3 = v ^ 3
      in
      if v3 > epsilon then
        v3

      else
        (116 * v - 16) / kappa

    fy = ( l + 16 ) / 116
    fx = a / 500 + fy
    fz =  fy - b / 200

    x = f fx
    z = f fz
    y =
      if l > kappa * epsilon then
        fy ^ 3

      else
        l / kappa

  in
  { x = x * xRef
  , y = y * yRef
  , z = z * zRef
  }


toRgb : Lab -> RGB
toRgb = toXyz >> Xyz.toRgb



-- Interpolate


interpolate lab1 lab2 time =
  { l = Interpolate.float lab1.l lab2.l time
  , a = Interpolate.float lab1.a lab2.a time
  , b = Interpolate.float lab1.b lab2.b time
  }
