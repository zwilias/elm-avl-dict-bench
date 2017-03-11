module Main exposing (main)

import Html exposing (Html)
import Html.Events as E
import Html.Attributes as A
import Rocket exposing ((=>))
import Benchmark exposing (..)
import RemoteData exposing (WebData)
import Dict
import Dict.Extra
import Set
import Http
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Badge as Badge
import Bootstrap.CDN
import Plot
import Plot.Line as Line
import Plot.Axis as Axis
import Plot.Label as Label
import Plot.Grid
import Markdown as M
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.program
        { init = init |> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = always Sub.none
        }


type alias GraphStates =
    Dict String Plot.State


type alias Model =
    { benches : WebData (List ReportListEntry)
    , filters : List FilterEntry
    , graphs : GraphStates
    }


type alias FilterEntry =
    { active : Bool, filter : BenchFilter }


type BenchFilter
    = OsFilter String String
    | BrowserFilter String String


filterToString : FilterEntry -> String
filterToString { active, filter } =
    case filter of
        OsFilter os version ->
            os ++ " - " ++ version

        BrowserFilter browser version ->
            browser ++ " - " ++ version


extractAllFilters : List BrowserInfo -> List FilterEntry
extractAllFilters infoList =
    let
        browserMap =
            Dict.Extra.groupBy (.browser) infoList
                |> Dict.map
                    (\_ browsers ->
                        List.map .version browsers
                            |> Set.fromList
                    )
                |> Dict.toList

        osMap =
            Dict.Extra.groupBy (.os) infoList
                |> Dict.map
                    (\_ oses ->
                        List.map .osVersion oses
                            |> Set.fromList
                    )
                |> Dict.toList
    in
        (List.concatMap
            (\( browser, versions ) ->
                Set.toList versions
                    |> List.map (\version -> BrowserFilter browser version)
            )
            browserMap
            ++ List.concatMap
                (\( os, versions ) ->
                    Set.toList versions
                        |> List.map (\version -> OsFilter os version)
                )
                osMap
        )
            |> List.map (FilterEntry False)


emptyModel : Model
emptyModel =
    { benches = RemoteData.Loading
    , filters = []
    , graphs = Dict.empty
    }


type Msg
    = ReceiveReports (WebData (List ReportListEntry))
    | ToggleFilter FilterEntry
    | PlotInteraction String (Plot.Interaction Msg)


fetchReports : Cmd Msg
fetchReports =
    Http.get "https://elm-bench.herokuapp.com/list.php" decodeReportList
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveReports


init : ( Model, List (Cmd Msg) )
init =
    emptyModel => [ fetchReports ]


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg model =
    case msg of
        ReceiveReports reports ->
            let
                filtered =
                    reports

                -- RemoteData.map (List.drop 1200) reports
            in
                { model
                    | benches = filtered
                    , filters =
                        filtered
                            |> RemoteData.withDefault []
                            |> List.map (.browserInfo)
                            |> extractAllFilters
                }
                    => []

        ToggleFilter filter ->
            let
                replacedFilters =
                    List.map
                        (\f ->
                            if f == filter then
                                { filter | active = not filter.active }
                            else
                                f
                        )
            in
                { model
                    | filters = replacedFilters model.filters
                }
                    => []

        PlotInteraction operation interaction ->
            case interaction of
                Plot.Internal internalMsg ->
                    { model
                        | graphs =
                            (Dict.update operation (\state -> Just <| Plot.update internalMsg (Maybe.withDefault Plot.initialState state)) model.graphs)
                    }
                        => []

                Plot.Custom _ ->
                    model => []


view : Model -> Html Msg
view model =
    Grid.container []
        [ renderHeader
        , renderFilters model.filters
        , renderReports model.graphs model.filters model.benches
        , Bootstrap.CDN.stylesheet
        , Html.node "link"
            [ A.href "style.css", A.rel "stylesheet", A.type_ "text/css" ]
            []
        , Html.node "link"
            [ A.href "https://cdn.rawgit.com/terezka/elm-plot/master/src/elm-plot.css"
            , A.rel "stylesheet"
            , A.type_ "text/css"
            ]
            []
        ]


renderHeader : Html Msg
renderHeader =
    Grid.row []
        [ Grid.col [] <|
            [ Html.h1 [] [ Html.text "Benchmarking Dict vs Dict.AVL" ]
            , Html.p [] [ Html.text "" ]
            ]
                ++ [ M.toHtml [] """
## Methodology.

Dictionaries are backed by trees. Whereas the core Dict is backed by a red-black tree, this dict is backed by an AVL tree. Both are instances of what's known as self-balancing binary search trees. I won't go into too much detail explaining SBBST's here, but what's important to know in order to follow the narrative, is that these trees need rebalancing operations after each mutation to keep themselves balanced. Being balanced is important to guarantee O(log n) behavior for lookups and mutations.

One thing that all such trees have in common, is that sequentially inserting nodes in ascending (or descending) order is a pathological case as it requires rebalancing pretty much constantly. As such, behavior for sequential insertion is a telling metric. Hence, we benchmark "serial insertion".

Since the performance of a single insert is not particularly interesting, we've opted to test performance over a series of inserts - for example building up a tree of 1000 sequential nodes. In code, this is calling `Dict.fromList` with a list of 1000 nodes. Since we want our graphs to count "insertions/second" rather than "toList operations per second", we multiple by the number of nodes. The overhead of the fold is negligible and used for both Dict and Dict.AVL.

Performance where nodes are inserted in a random order tend to suffer from less rebalancing overhead. Since people will often construct Dicts from a List, and these Lists may or may not be sorted, it is important to check both. Hence, we also benchmark "random insertion".

Deletion is often much more tricky than insertion, requiring more complex operations. Here, we've used separate methodologies for the sequential and the randomized removal benchmarks. In the case of a sequential benchmark, we start out with a Dict of a certain size, and we fold over that, deleting it key by key, sequentially. In the case of randomized removal, we start with a dict of a certain size and a separate list of equal size, seeded from a separate seed, but constrained to a similar domain so that there is bound to be some overlap. In particular, it can be noted that for size 1, we're deleting a key that does not exist, which is a no-op and results in the odd discrepancy in performance between sequential and randomized removal.

What we've called `enumeration` here is really just `Dict.toList`, starting from a dict of some given size. I figure since it's a fairly common operation, it would warrant having its own benchmark here, too.

Finally, there's the sequential retrieval benchmark, where we fold over all the keys in the Dict, and retrieve its value using `Dict.get`. This gets us the amortized, average cost.

Note that each benchmark listed here, is the average of running that benchmark over Dicts with different key types, namely all the comparable types, as well as a tuple and a list of 2 comparables. We do this because comparison operations tend to perform wildly different for some of these types - a simple `>` or `<` will be faster for `Int` or `String`, while a `Char` basically needs a `case compare a b of` if there are more than 2 branches (and thus, comparisons) needed.

These benchmarks were gathered thanks to the lovely Elm community who gave up their time by surfing to [https://ilias.xyz/elm-avl-dict-exploration](https://ilias.xyz/elm-avl-dict-exploration) and running the benchmarks. Quite a few of these benchmarks were also run on a variety of browser/platform combinations using [BrowerStack](https://www.browserstack.com), who generously provided me with a free OSS license to use their infrastructure.

## Graphs and tables.

As mentioned above, for each type of benchmark, we run the benchmark on dictionaries of different *size*s, on a logarithmic scale.

For each of these sizes, we look at all the samples that were submitted for that particular size. `65 samples` in this case means we got 65 (completed) submissions for a particular operation/size pair. A higher number of *sample*s means a higher accuracy, taking into account more browsers and platforms.

The next two columns count the number of relevant operations per second we managed to execute. As such, higher is better. As noted in the methodology section above, this is the raw data multiplied by the size of the dictionary, so we can safely go from _number of `fromList`_ operations to _number of `insert`_ operations. This does include a negligible overhead from the fold calls these operations are based on.

The *delta*, finally, measures the performance delta as a percentage. `2%` in this case means that `Dict.AVL` performs 2% _more_ operations than `Dict`.

## Browsers and platforms

Note that you can filter out all samples from a certain browser or platform by toggling the badge. Since any sample that matches a disabled badge will be filtered out, mixing browser and platform filters will probably give you some odd/unexpected results :)
    """ ]
        ]


renderReports :
    GraphStates
    -> List FilterEntry
    -> WebData (List ReportListEntry)
    -> Html Msg
renderReports graphs filters data =
    case data of
        RemoteData.NotAsked ->
            Html.text "Apparently, I forgot to ask for my data. Oops?"

        RemoteData.Loading ->
            Html.text "loading the benchmarks..."

        RemoteData.Failure e ->
            Html.text <| toString e

        RemoteData.Success benches ->
            Html.div []
                [ renderBenches graphs filters benches
                ]


renderFilters : List FilterEntry -> Html Msg
renderFilters filterEntryList =
    Html.ul [ A.class "tags" ] <|
        List.map
            (\f -> Html.li [ A.class "tag" ] [ renderFilter f ])
            filterEntryList


renderFilter : FilterEntry -> Html Msg
renderFilter filterEntry =
    let
        handler =
            E.onClick <| ToggleFilter filterEntry

        desc =
            filterToString filterEntry
    in
        case filterEntry.active of
            True ->
                Badge.badgeDanger [ handler ] [ Html.text desc ]

            False ->
                case filterEntry.filter of
                    OsFilter _ _ ->
                        Badge.badgePrimary [ handler ] [ Html.text desc ]

                    BrowserFilter _ _ ->
                        Badge.badgeSuccess [ handler ] [ Html.text desc ]


filterMatch : BenchFilter -> BrowserInfo -> Bool
filterMatch filter browserInfo =
    case filter of
        OsFilter os version ->
            os == browserInfo.os && version == browserInfo.osVersion

        BrowserFilter browser version ->
            browser == browserInfo.browser && version == browserInfo.version


filterReport : List FilterEntry -> ReportListEntry -> Bool
filterReport filters { browserInfo } =
    List.all (not << .active) filters
        || List.all
            (\filter ->
                (not filter.active) || (not <| filterMatch filter.filter browserInfo)
            )
            filters


renderBenches :
    GraphStates
    -> List FilterEntry
    -> List ReportListEntry
    -> Html Msg
renderBenches graphs filters benches =
    Html.div [] <|
        (benches
            |> Dict.Extra.groupBy (.operation)
            |> Dict.toList
            |> List.concatMap
                (\( operation, reportListEntries ) ->
                    let
                        groupedReports =
                            reportListEntries
                                |> Dict.Extra.groupBy (.size)
                                |> Dict.toList
                    in
                        [ Grid.row []
                            [ Grid.col []
                                [ Html.h2 []
                                    [ Html.text operation ]
                                ]
                            ]
                        , Grid.row
                            []
                            [ Grid.col
                                [ Col.lg4 ]
                                [ renderGroupedReports
                                    filters
                                    groupedReports
                                ]
                            , Grid.col [ Col.lg8 ]
                                [ renderGraph
                                    (Dict.get operation graphs |> Maybe.withDefault Plot.initialState)
                                    filters
                                    groupedReports
                                    |> Html.map (PlotInteraction operation)
                                ]
                            ]
                        ]
                )
        )


renderGroupedReports : List FilterEntry -> List ( Int, List ReportListEntry ) -> Html Msg
renderGroupedReports filters groupedReports =
    Html.table [] <|
        [ Html.tr []
            [ Html.th [] [ Html.text "size" ]
            , Html.th [] [ Html.text "samples" ]
            , Html.th [] [ Html.text "Dict" ]
            , Html.th [] [ Html.text "Dict.AVL" ]
            , Html.th [] [ Html.text "delta" ]
            ]
        ]
            ++ List.map
                (\( size, reports ) ->
                    reports
                        |> List.filter
                            (filterReport filters)
                        |> List.map (.report)
                        |> combineReports
                        |> renderReport size
                )
                groupedReports


renderGraph :
    Plot.State
    -> List FilterEntry
    -> List ( Int, List ReportListEntry )
    -> Html (Plot.Interaction Msg)
renderGraph state filters reports =
    let
        extract extrator =
            reports
                |> List.map
                    (\( size, entries ) ->
                        let
                            multiplier =
                                toFloat size

                            result =
                                entries
                                    |> List.filter (filterReport filters)
                                    |> List.map (.report)
                                    |> combineReports
                                    |> (\report ->
                                            averageOpsPerSec
                                                (List.map
                                                    extrator
                                                    report.comparisons
                                                )
                                       )
                        in
                            ( logBase 10 multiplier, result * multiplier )
                    )

        left : List Plot.Point
        left =
            extract .left

        right : List Plot.Point
        right =
            extract .right

        upperBound =
            max
                (List.foldl (\( _, y ) m -> max y m) 0 left)
                (List.foldl (\( _, y ) m -> max y m) 0 right)

        lowerBound =
            min
                (List.foldl (\( _, y ) m -> min y m) 0 left)
                (List.foldl (\( _, y ) m -> min y m) 0 right)

        yTick : Float
        yTick =
            ((upperBound - lowerBound)
                / 4
            )
                |> logBase 10
                |> ceiling
                |> (\n -> 10 ^ n)
                |> (\n -> (toFloat n) / 2)
    in
        Plot.plotInteractive
            [ Plot.size ( 760, 200 )
            , Plot.margin ( 0, 30, 30, 90 )
            ]
            [ Plot.line
                [ Line.stroke "#ff9edf"
                , Line.strokeWidth 2
                ]
                left
            , Plot.line
                [ Line.stroke "#cfd8ea"
                , Line.strokeWidth 2
                ]
                right
            , Plot.xAxis
                [ Axis.line
                    [ Line.stroke "#949494"
                    ]
                , Axis.tickDelta 1
                , Axis.label
                    [ Label.format (.value >> (^) 10 >> toString) ]
                ]
            , Plot.yAxis
                [ Axis.line [ Line.stroke "#949494" ]
                , Axis.tickDelta yTick
                ]
            , Plot.horizontalGrid [ Plot.Grid.lines [ Line.opacity 0.1 ] ]
            , Plot.verticalGrid [ Plot.Grid.lines [ Line.opacity 0.1 ] ]
            , Plot.hint [] (Plot.getHoveredValue state)
            ]


combineReports : List Report -> Report
combineReports reportList =
    case reportList of
        [] ->
            { name = "", count = 0, comparisons = [] }

        head :: tail ->
            let
                combinedComparisons =
                    List.concatMap (.comparisons) reportList
            in
                { name = head.name
                , count = List.length reportList
                , comparisons = combinedComparisons
                }


renderReport : Int -> Report -> Html Msg
renderReport size report =
    let
        multiplier =
            toFloat size

        left =
            averageOpsPerSec (List.map .left report.comparisons)

        right =
            averageOpsPerSec (List.map .right report.comparisons)
    in
        Html.tr []
            [ Html.th [] [ Html.text <| toString size ]
            , Html.th [] [ Html.text <| toString report.count ]
            , Html.td []
                [ Html.text <| toString <| round <| left * multiplier ]
            , Html.td []
                [ Html.text <| toString <| round <| right * multiplier ]
            , Html.td []
                [ Html.text <|
                    (toString <|
                        round <|
                            ((right / left)
                                * 100
                            )
                                - 100
                    )
                        ++ "%"
                ]
            ]


averageOpsPerSec : List Benchmark -> Float
averageOpsPerSec benches =
    List.map opsPerSec benches
        |> List.sum
        |> (\sum -> sum / (toFloat <| List.length benches))


countOperations : Benchmark -> Int
countOperations benchmark =
    List.length benchmark.samples * benchmark.sampleSize


totalTime : Benchmark -> Float
totalTime benchmark =
    (List.foldl ((+)) 0 benchmark.samples)


opsPerSec : Benchmark -> Float
opsPerSec benchmark =
    (toFloat <| countOperations benchmark) * 1000 / totalTime benchmark
