module Color.OkLch exposing (..)

import Color.Interpolate as Interpolate


type alias OkLch =
    { l : Float
    , c : Float
    , h : Float
    , a : Maybe Float
    }



-- Interpolate


interpolate : OkLch -> OkLch -> Float -> OkLch
interpolate a b time =
    { l = Interpolate.float a.l b.l time
    , c = Interpolate.float a.c b.c time
    , h = Interpolate.hue a.h b.h time
    , a = Maybe.map2 (\aa ba -> Interpolate.float aa ba time) a.a b.a
    }


interpolateLong : OkLch -> OkLch -> Float -> OkLch
interpolateLong a b time =
    { l = Interpolate.float a.l b.l time
    , c = Interpolate.float a.c b.c time
    , h = Interpolate.normalizeHue (Interpolate.float a.h b.h time)
    , a = Maybe.map2 (\aa ba -> Interpolate.float aa ba time) a.a b.a
    }



-- Serialize


toString : OkLch -> String
toString { l, c, h, a } =
    "oklch("
        ++ String.fromFloat l
        ++ "% "
        ++ String.fromFloat c
        ++ " "
        ++ String.fromFloat h
        ++ (case a of
                Just aa ->
                    " / " ++ String.fromFloat aa

                Nothing ->
                    ""
           )
        ++ ")"
