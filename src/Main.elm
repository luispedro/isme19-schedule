module Main exposing (..)

import Browser
import Json.Decode as J
import Set as S

import Html
import Http
import Task
import Time
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

type alias Talk =
    { day : String
    , time : String
    , speaker : Maybe String
    , room : String
    , title : String
    , session : String
    , abstract : Maybe String
    }

decodeTalk : J.Decoder Talk
decodeTalk = J.map7 Talk
    (J.field "Date" J.string)
    (J.field "Timespan" J.string)
    (J.field "Speaker" (J.nullable J.string))
    (J.field "Room" J.string)
    (J.field "Title" J.string)
    (J.field "Session" J.string)
    (J.field "Abstract" (J.nullable J.string))

type SortOrder =
    ByTime
    | ByAuthor
    | BySession


type alias SimpleTime = { day : Int, hour : Int }

getSimpleTime : Time.Posix -> SimpleTime
getSimpleTime t =
    let
        hourUTC = Time.toHour Time.utc t
        minUTC = Time.toMinute Time.utc t
        looseHourUTC =
            if minUTC < 15
            then hourUTC - 1
            else hourUTC
    in
        { day = Time.toDay Time.utc t
        , hour = 2 + looseHourUTC -- 2 + is to adjust to French time
        }

type alias FilterSet =
    { days : S.Set String
    , session : String
    , sessionFilterState : Dropdown.State
    , speaker : String
    , title : String
    , abstract : String
    , showFullAbstract : Bool
    , sortOrder : SortOrder
    , now : SimpleTime
    , showPastTalks : Bool
    , talks : List Talk
    }

initFilters talks =
    { days = talks
                |> List.map .day
                |> S.fromList
    , session = ""
    , sessionFilterState = Dropdown.initialState
    , speaker = ""
    , title = ""
    , abstract = ""
    , showFullAbstract = False
    , sortOrder = ByTime
    , now = { day = 23, hour = 12 }
    , showPastTalks = False
    , talks = talks
    }

type Model =
    Loading
    | LoadFailed String
    | ShowTalks FilterSet

main = let
        getTalks : Cmd Msg
        getTalks = Http.get
            { url = "ISMB_ECCB_2023_All_sessions.json"
            , expect = Http.expectJson GotData (J.list decodeTalk)
            }
    in Browser.element
        { init = \() -> (Loading, getTalks)
        , update = update
        , subscriptions = \model -> case model of
            Loading -> Sub.none
            LoadFailed _ -> Sub.none
            ShowTalks m -> Dropdown.subscriptions m.sessionFilterState SessionFilterChanged
        , view = view
        }

type Msg =
    GotData (Result Http.Error (List Talk))
    | CurTime Time.Posix
    | ToggleDayFilter String
    | SetSessionFilter String
    | SessionFilterChanged Dropdown.State
    | ToggleShowPastTalks
    | ToggleShowFullAbstract
    | SetSortOrder SortOrder
    | UpdateTitleFilter String
    | UpdateSpeakerFilter String
    | UpdateAbstractFilter String

update msg model =
    let nmodel = case msg of
            GotData r -> case r of
              Ok d -> ShowTalks <| initFilters d
              Err err -> LoadFailed <| case err of
                Http.BadUrl e -> "BadURL: " ++ e
                Http.Timeout -> "TimeOut"
                Http.NetworkError -> "NetworkError"
                Http.BadStatus c -> "BadStatus: " ++ String.fromInt c
                Http.BadBody e -> "BadBody: " ++ e
            _ -> case model of
                Loading -> model
                LoadFailed err -> model
                ShowTalks m -> case msg of
                    GotData _ -> model -- impossible, but needed to satisfy the compiler
                    CurTime t -> ShowTalks <| adjustDays { m | now = getSimpleTime t}
                    ToggleDayFilter d ->
                        let
                            newSet =
                                if S.member d m.days
                                then S.remove d m.days
                                else S.insert d m.days
                        in ShowTalks { m | days = newSet }
                    SetSessionFilter d -> ShowTalks { m | session = d }
                    SessionFilterChanged s -> ShowTalks { m | sessionFilterState = s }
                    ToggleShowFullAbstract -> ShowTalks { m | showFullAbstract = not m.showFullAbstract }
                    ToggleShowPastTalks -> ShowTalks <| adjustDays { m | showPastTalks = not m.showPastTalks }
                    SetSortOrder o -> ShowTalks { m | sortOrder = o }
                    UpdateTitleFilter t -> ShowTalks { m | title = t }
                    UpdateSpeakerFilter t -> ShowTalks { m | speaker = t }
                    UpdateAbstractFilter t ->  ShowTalks { m | abstract = t }
        cmd = case msg of
            GotData (Ok _) -> Task.perform CurTime Time.now
            _ -> Cmd.none
    in (nmodel, cmd)

hasPassed : Talk -> SimpleTime -> Bool
hasPassed t now = talkDay t.day < now.day || talkDay t.day == now.day && talkHour t.time < now.hour

adjustDays : FilterSet -> FilterSet
adjustDays m =
    if m.showPastTalks
    then { m | days = List.map .day m.talks |> S.fromList }
    else let
            asDayString : Int -> String
            asDayString d = case d of
                23 -> "July 23rd"
                24 -> "July 24th"
                25 -> "July 25th"
                26 -> "July 26th"
                27 -> "July 27th"
                _ -> "x"
            ndays : S.Set String
            ndays = List.foldl (\d active -> S.remove (asDayString d) active) m.days (List.range 23 (m.now.day - 1))
        in { m | days = ndays }


talkDay t = case String.split " " t of
    [_, n] -> String.left 2 n |> String.toInt |> Maybe.withDefault 0
    _ -> 0

talkHour t =
    let
        (h, _) = parseTime t
    in h

view m =
    Html.div []
        [ CDN.stylesheet
        , CDN.fontAwesome
        , Html.node "link"
            [ HtmlAttr.rel "stylesheet"
            , HtmlAttr.href "style.css"
            ]
            []
        , viewModel m ]


parseTime : String -> (Int, Int)
parseTime t =
    let
        parts = t
                |> String.split "-"
                |> List.map String.trim
                |> List.head
                |> Maybe.withDefault ""
                |> String.split ":"
                |> List.map String.trim
    in
        ( parts
            |> List.head
            |> Maybe.andThen String.toInt
            |> Maybe.withDefault 0
        , parts
            |> List.tail
            |> Maybe.andThen List.head
            |> Maybe.andThen String.toInt
            |> Maybe.withDefault 0
        )

viewModel model = case model of
    Loading -> Html.text "Loading..."
    LoadFailed err -> Html.text ("Load error: " ++ err)
    ShowTalks m ->
        let
            filterDays = List.filter (\t -> S.member t.day m.days)
            filterSpeakers = List.filter (\t -> String.contains (String.toLower m.speaker) (String.toLower (Maybe.withDefault "" t.speaker)))
            filterTitles = List.filter (\t -> String.contains (String.toLower m.title) (String.toLower t.title))
            filterAbstracts = List.filter (\t -> String.contains (String.toLower m.abstract) (String.toLower (Maybe.withDefault "" t.abstract)))
            filterSessions = List.filter (\t -> m.session == "" || m.session == t.session)
            filterPastTalks = List.filter (\t -> m.showPastTalks || not (hasPassed t m.now))
            sel = m.talks
                    |> filterDays
                    |> filterTitles
                    |> filterSpeakers
                    |> filterAbstracts
                    |> filterSessions
                    |> filterPastTalks
                    |> (case m.sortOrder of
                        ByTime -> List.sortBy (\t -> (t.day, parseTime t.time, t.session))
                        ByAuthor -> List.sortBy (\t -> (Maybe.withDefault "ZZZ" t.speaker, t.day, parseTime t.time))
                        BySession -> List.sortBy (\t -> (t.session, t.day, parseTime t.time))
                        )
            allDays = List.map (\t -> t.day) m.talks
                        |> S.fromList
                        |> S.toList
                        |> List.sort
            allSessions = m.talks
                        |> List.map .session
                        |> S.fromList
                        |> S.toList
                        |> List.sort
        in Grid.containerFluid []
            [ Grid.simpleRow
                [ Grid.col []
                    [ Html.h1 [] [ Html.text "ISMB/ECCB 2023" ]
                    , Html.p [] [ Html.text "This is a list of all sessions at ISMB/ECCB 2023."]
                    , Html.p [] [ Html.text "Created by "
                                , Html.a [ HtmlAttr.href "https://luispedro.org/"]
                                    [ Html.text "Luis Pedro Coelho" ]
                                , Html.text " based on a table from "
                                , Html.a [ HtmlAttr.href "https://twitter.com/larsjuhljensen"]
                                    [ Html.text "Lars Juhl Jensen" ]
                                , Html.text ". Code is available on "
                                , Html.a [ HtmlAttr.href "https://github.com/luispedro/ismb-schedule" ]
                                    [ Html.text "GitHub" ]
                                ]
                    ]
                ]
            , Grid.simpleRow
                    [ Grid.col [ ]
                        (((Html.h4 [] [Html.text "Filter by days" ])::
                        List.map (\d ->
                                Button.button
                                        [ (if S.member d m.days then Button.primary else Button.outlineSecondary)
                                        , Button.onClick (ToggleDayFilter d)
                                        ]
                                        [ Html.text d ]
                            ) allDays) ++ [Button.button
                                [ (if m.showPastTalks then Button.primary else Button.outlineSecondary)
                                , Button.onClick ToggleShowPastTalks
                                ]
                                [ Html.text (if m.showPastTalks then "Hide past talks" else "Show past talks") ]])
                    , Grid.col [ ]
                        (let
                            filter =
                                allSessions
                                        |> List.map (\s ->
                                                Dropdown.buttonItem
                                                    [ HE.onClick (SetSessionFilter s)
                                                    ]
                                                    [ Html.text s ]
                                                )
                        in [Html.h4 [] [Html.text "Filter by session" ]
                            , Dropdown.dropdown
                                m.sessionFilterState
                                { options = [ Dropdown.alignMenuRight ]
                                , toggleMsg = SessionFilterChanged
                                , toggleButton =
                                    Dropdown.toggle [ ] [ Html.text "Select session" ]
                                , items =
                                    ([ Dropdown.buttonItem [ HE.onClick (SetSessionFilter "") ] [ Html.text "All sessions" ]
                                    , Dropdown.divider ] ++ filter)
                                }
                            , if m.session /= ""
                                then Html.p [] [ Html.text "Currently showing only: ", Html.i [] [ Html.text m.session ] ]
                                else Html.p [] [ ]
                            ])
                    , Grid.col [ ]
                        [Html.h4 [] [Html.text "Filter by speaker" ]
                        ,Html.input [ HtmlAttr.type_ "text", HtmlAttr.value m.speaker, HE.onInput UpdateSpeakerFilter ] []
                        ]
                    , Grid.col [ ]
                        [Html.h4 [] [Html.text "Filter by title" ]
                        ,Html.input [ HtmlAttr.type_ "text", HtmlAttr.value m.title, HE.onInput UpdateTitleFilter ] []
                        ]
                    , Grid.col [ ]
                        [Html.h4 [] [Html.text "Filter by abstract" ]
                        ,Html.input [ HtmlAttr.type_ "text", HtmlAttr.value m.abstract, HE.onInput UpdateAbstractFilter ] []
                        ]
                    ]

            , Grid.simpleRow
                    [ Grid.col [ ]
                [ Html.p [] [ Html.text ("Showing " ++ String.fromInt (List.length sel) ++ " talks") ]
                , Button.button
                    [ (if m.showFullAbstract then Button.primary else Button.outlineSecondary)
                    , Button.onClick ToggleShowFullAbstract
                    ]
                    [ Html.text (if m.showFullAbstract then "Trim abstracts" else "Expand abstracts") ]
                , Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead =  Table.simpleThead
                        [ Table.th [ Table.cellAttr <| HE.onClick (SetSortOrder ByTime) ] [ Html.a [HtmlAttr.href "#" ] [ Html.text "When?" ] ]
                        , Table.th [] [ Html.text "Room" ]
                        , Table.th [ Table.cellAttr <| HE.onClick (SetSortOrder ByAuthor) ] [ Html.a [HtmlAttr.href "#" ] [ Html.text "Speaker" ] ]
                        , Table.th [] [ Html.text "Title" ]
                        , Table.th [ Table.cellAttr <| HE.onClick (SetSortOrder BySession) ] [ Html.a [HtmlAttr.href "#" ] [ Html.text "Session" ] ]
                        , Table.th [] [ Html.text "Abstract" ]
                        ]
                    , tbody =
                        sel
                            |> List.map (\t ->
                                Table.tr []
                                    [ Table.td [] [ Html.text <| t.day ++ " " ++ t.time ]
                                    , Table.td [] [ Html.text t.room ]
                                    , Table.td [] [ showHits m.speaker (Maybe.withDefault "" t.speaker) ]
                                    , Table.td [] [ showHits m.title t.title ]
                                    , Table.td [] [ Html.text t.session ]
                                    , Table.td [] [ showHits m.abstract <| trimAbstract m.showFullAbstract (Maybe.withDefault "" t.abstract) ]
                                    ])
                            |> Table.tbody []
                    }
                ]
            ]
        ]


trimAbstract : Bool -> String -> String
trimAbstract showFull abstract =
    if showFull
    then abstract
    else
        let
            maxLen = 150
            trimmed = String.left maxLen abstract
        in
        if String.length abstract <= maxLen
        then abstract
        else trimmed ++ "..."
showHits filter abstract =
    if filter == ""
    then Html.text abstract
    else
    let
        matches : List Int
        matches = String.indexes (String.toLower filter) (String.toLower abstract)
        showMatch : Int -> Int -> Html msg
        showMatch start ix =
            let
                before = String.slice start ix abstract
            in
                Html.span []
                    [Html.text before
                    ,Html.strong [] [Html.text <| String.slice ix (ix + String.length filter) abstract]]
        showMatches : Int -> List Int -> List (Html msg)
        showMatches start ixs = case ixs of
            [] -> [Html.text <| (String.slice start (String.length abstract) abstract)]
            ix::rest -> showMatch start ix :: showMatches (ix + String.length filter) rest
    in
    Html.p []
        (showMatches 0 matches)

