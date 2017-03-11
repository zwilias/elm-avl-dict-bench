module Benchmark exposing (..)

import Json.Decode
    exposing
        ( string
        , list
        , Decoder
        , at
        , succeed
        , int
        , float
        , andThen
        , nullable
        )
import Json.Decode.Pipeline exposing (decode, required, requiredAt, hardcoded)
import Maybe.Extra as M


type alias ReportListEntry =
    { browserInfo : BrowserInfo
    , report : Report
    , size : Int
    , operation : String
    }


type alias BrowserInfo =
    { browser : String
    , version : String
    , os : String
    , osVersion : String
    }


type alias Report =
    { name : String
    , count : Int
    , comparisons : List Comparison
    }


type alias Comparison =
    { name : String
    , left : Benchmark
    , right : Benchmark
    }


type alias Benchmark =
    { name : String
    , sampleSize : Int
    , samples : List Float
    }


decodeReportList : Decoder (List ReportListEntry)
decodeReportList =
    list (nullable decodeReportListEntry)
        |> andThen (M.values >> decode)


decodeReportListEntry : Decoder ReportListEntry
decodeReportListEntry =
    decode ReportListEntry
        |> required "browserInfo" decodeBrowserInfo
        |> required "benchmark" decodeReport
        |> required "size" int
        |> required "type" string


decodeBrowserInfo : Decoder BrowserInfo
decodeBrowserInfo =
    decode BrowserInfo
        |> required "browser" string
        |> required "version" string
        |> required "os" string
        |> required "osVersion" string


decodeReport : Decoder Report
decodeReport =
    let
        decoder =
            decode Report
                |> required "name" (string)
                |> hardcoded 1
                |> required "benchmarks" (list comparisonDecoder)
    in
        at [ "report" ] (decoder)


comparisonDecoder : Decoder Comparison
comparisonDecoder =
    decode Comparison
        |> required "name" string
        |> required "a" benchmarkDecoder
        |> required "b" benchmarkDecoder


benchmarkDecoder : Decoder Benchmark
benchmarkDecoder =
    decode Benchmark
        |> required "name" string
        |> requiredAt [ "status", "sampleSize" ] int
        |> requiredAt [ "status", "samples" ] (list float)
