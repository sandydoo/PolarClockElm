module Color.Interpolate exposing (..)

import Array
import Cons exposing (Cons(..))


constant : a -> t -> a
constant a _ =
    a


linear : Float -> Float -> Float -> Float
linear a d t =
    a + d * t


float : Float -> Float -> Float -> Float
float start end time =
    linear start (end - start) time


hue : Float -> Float -> Float -> Float
hue a b =
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


scaleProgress : Int -> Float -> ( Int, Int, Float )
scaleProgress max time =
    let
        position =
            time * toFloat (max - 1)

        bottom =
            floor position

        ( from, to ) =
            if bottom == max then
                ( bottom - 1, bottom )

            else
                ( bottom, bottom + 1 )

        intermediateProgress =
            position - toFloat from
    in
    ( from, to, intermediateProgress )


takeTwoOr : a -> List a -> ( a, a )
takeTwoOr fallback list =
    case list of
        [] ->
            ( fallback, fallback )

        [ a ] ->
            ( a, a )

        a :: b :: _ ->
            ( a, b )


manyVia : (color -> color -> Float -> color) -> Cons color -> (Float -> color)
manyVia interpolate (Cons baseColor colorsL) =
    let
        colors =
            Array.fromList (baseColor :: colorsL)

        count =
            Array.length colors

        toCurrentProgress =
            scaleProgress count

        listOfHelper : Float -> color
        listOfHelper totalProgress =
            let
                ( from, to, progress ) =
                    toCurrentProgress totalProgress

                ( color1, color2 ) =
                    takeTwoOr baseColor
                        << Array.toList
                    <|
                        Array.slice from (to + 1) colors
            in
            interpolate color1 color2 progress
    in
    listOfHelper
