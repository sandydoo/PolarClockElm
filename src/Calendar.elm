module Calendar exposing
    ( Date
    , DateTime(..)
    , Unit(..)
    , daysRange
    , fromPosix
    , hoursRange
    , minutesRange
    , monthsRange
    , range
    , secondsRange
    , toDatetime
    , toDay
    , toMonth
    , toPart
    , toWeekday
    , weekdaysRange
    )

import Date exposing (Interval(..))
import Time exposing (Month(..), Weekday(..))


type alias Date =
    Date.Date


type Unit
    = Months
    | Weekdays
    | Days
    | Hours
    | Minutes
    | Seconds


fromPosix =
    Date.fromPosix


type DateTime
    = DateTime Date.Date Time.Zone Time.Posix


toDatetime : Time.Zone -> Time.Posix -> DateTime
toDatetime zone time =
    DateTime (fromPosix zone time) zone time


monthToNumber : Month -> Int
monthToNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


numberToMonth : Int -> Month
numberToMonth n =
    case n of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


weekdayToNumber : Weekday -> Int
weekdayToNumber weekday =
    case weekday of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            7


numberToWeekday : Int -> Weekday
numberToWeekday n =
    case n of
        1 ->
            Mon

        2 ->
            Tue

        3 ->
            Wed

        4 ->
            Thu

        5 ->
            Fri

        6 ->
            Sat

        _ ->
            Sun



-- Extract parts from dates


toPart : Unit -> DateTime -> Int
toPart unit (DateTime _ zone time) =
    case unit of
        Months ->
            toMonth zone time

        Weekdays ->
            toWeekday zone time

        Days ->
            toDay zone time

        Hours ->
            Time.toHour zone time

        Minutes ->
            Time.toMinute zone time

        Seconds ->
            Time.toSecond zone time


toWeekday : Time.Zone -> Time.Posix -> Int
toWeekday zone time =
    time
        |> Time.toWeekday zone
        |> weekdayToNumber
        |> decrement


toMonth : Time.Zone -> Time.Posix -> Int
toMonth zone time =
    time
        |> Time.toMonth zone
        |> monthToNumber
        |> decrement


toDay : Time.Zone -> Time.Posix -> Int
toDay zone =
    Time.toDay zone >> decrement


decrement : Int -> Int
decrement n =
    n - 1



-- Create ranges for labels and ticks


range : Unit -> DateTime -> List String
range unit datetime =
    case unit of
        Months ->
            monthsRange

        Weekdays ->
            weekdaysRange

        Days ->
            daysRange datetime

        Hours ->
            hoursRange

        Minutes ->
            minutesRange

        Seconds ->
            secondsRange


secondsRange : List String
secondsRange =
    List.map String.fromInt (List.range 0 59)


minutesRange : List String
minutesRange =
    List.map String.fromInt (List.range 0 59)


hoursRange : List String
hoursRange =
    List.map String.fromInt (List.range 0 23)


daysRange : DateTime -> List String
daysRange (DateTime date _ _) =
    let
        year =
            Date.year date

        month =
            Date.month date

        start =
            Date.fromCalendarDate year month 1

        until =
            start |> Date.add Date.Months 1
    in
    List.map (Date.day >> String.fromInt) (Date.range Day 1 start until)


weekdaysRange : List String
weekdaysRange =
    List.map (numberToWeekday >> weekdayToString) (List.range 1 7)


monthsRange : List String
monthsRange =
    List.map (numberToMonth >> monthToString) (List.range 1 12)
